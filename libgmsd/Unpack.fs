namespace libgmsd

open System
open System.IO

/// Functions related to unpacking a data.win file and extracting the
/// raw code, strings, and references. A `Form` value represents the
/// result of this process.

module Unpack =
    type Script =
        {NameAddress: StringAddress;
         Start: CodeAddress;
         Length: int32;
         Bytecode: uint32[]}

    type Form =
        {Code: Script[];
         ReferenceMap: Map<ReferenceAddress, StringAddress>;
         StringAddressMap: Map<StringAddress, string>;
         StringIndexMap: Map<uint32, string>}

    // TODO: Lots of these functions make more sense as methods of some class
    // that wraps around BinaryReader. Maybe implement that some day? Anyway,
    // keep in mind that they modify `br`'s internal state.

    /// Parse the header of some expected chunk, returning its length.
    let parseChunkHeader (br : BinaryReader) (expectedName : string) : int32 =
        let b = new System.String(br.ReadChars 4)
        assert (b = expectedName)
        br.ReadInt32()

    /// Read the CODE chunk of a data.win file.
    let readCode (br : BinaryReader) : Script[] =
        let chunkLength = parseChunkHeader br "CODE"
        let addressCount = br.ReadInt32()
        // Skip address array.
        br.BaseStream.Seek(int64 addressCount * 4L, SeekOrigin.Current) |> ignore

        Array.init addressCount (fun _ ->
            let nameAddress = br.ReadUInt32()
            let length = br.ReadInt32()
            let start = uint32 br.BaseStream.Position
            let code = Array.init (length / 4) (fun _ -> br.ReadUInt32())
            {NameAddress = nameAddress;
             Start = start;
             Length = length;
             Bytecode = code}
        )

    /// Make a Map from each occurrence of a reference to its name address. 
    /// Call this with `br` pointing at the start of `ReferenceDefinitions`.
    let readReferenceMap (br : BinaryReader) (chunkName : string) : Map<ReferenceAddress, StringAddress> =
        let chunkLength = parseChunkHeader br chunkName
        let numEntries = chunkLength / 12
        
        Array.init numEntries (fun _ ->
            let name = br.ReadUInt32()
            let occurrences = br.ReadInt32()
            let firstReference = br.ReadUInt32()
            let nextEntry = br.BaseStream.Position

            br.BaseStream.Seek(int64 firstReference, SeekOrigin.Begin) |> ignore

            let result =
                Seq.toArray <| Seq.unfold (fun n ->
                    if n = 0
                        then None
                        else
                            // We're pointing at the Pop instruction containing the next
                            // use of this variable. Skip the first word...
                            let addr = br.BaseStream.Position
                            br.ReadUInt32() |> ignore
                            // And read the offset out of the reference word that follows.
                            let offset = br.ReadUInt32() &&& 0x00FFFFFFu

                            br.BaseStream.Seek(int64 offset - 8L, SeekOrigin.Current) |> ignore
                            Some ((uint32 addr, name), n - 1)
                ) occurrences

            br.BaseStream.Seek(nextEntry, SeekOrigin.Begin) |> ignore
            Array.append [| firstReference, name |] result
        ) |> Seq.concat |> Map.ofSeq

    /// Read the STRG chunk of a data.win file.
    let readStrings (br : BinaryReader) : Map<uint32, string> * Map<StringAddress, string> =
        let chunkLength = parseChunkHeader br "STRG"
        let endPosition = br.BaseStream.Position + int64 chunkLength

        let addressCount = br.ReadInt32()
        let addresses = Array.init addressCount (fun _ -> br.ReadInt32())

        let startPosition = br.BaseStream.Position 
        let stringMaps : Map<uint32, string> * Map<StringAddress, string> =
            let (stringIndexArray, stringAddressArray) =
                Array.unzip <| Array.init addressCount (fun idx ->
                    // Read the idx-th entry.
                    let len = br.ReadInt32()
                    let pos = uint32 (br.BaseStream.Position)
                    let str = new System.String(br.ReadChars len)
                    
                    // Skip the terminating zero byte.
                    br.ReadChar() |> ignore
                    
                    ((uint32 idx, str), (pos, str))
                )
            (Map.ofArray stringIndexArray,
             Map.ofArray stringAddressArray)

        br.BaseStream.Seek(endPosition, SeekOrigin.Begin) |> ignore
        stringMaps

    /// Assert we're pointing at a chunk, then skip over it.
    let skipChunk (br : BinaryReader) (expectedName : string) : unit =
        printfn "Skipping chunk %s..." expectedName
        let chunkLength = parseChunkHeader br expectedName
        printfn "With length %d." chunkLength
        br.BaseStream.Seek(int64 chunkLength, SeekOrigin.Current) |> ignore
        
    /// Read an entire data.win file, returning the data in its root (FORM) chunk.
    let readForm (path : string) : Form =
        // Open the file in binary read mode.
        let fs = new FileStream(path, FileMode.Open)
        let br = new BinaryReader(fs)
        
        // Read the root chunk.
        let formLength = parseChunkHeader br "FORM"
        
        // Chunks we're not interested in:
        skipChunk br "GEN8" // Skip metadata
        skipChunk br "OPTN" // Empty
        skipChunk br "EXTN" // Empty
        skipChunk br "SOND" // Skip sound data
        skipChunk br "AGRP" // Empty
        skipChunk br "SPRT" // Skip sprites
        skipChunk br "BGND" // Skip backgrounds
        skipChunk br "PATH" // Skip paths
        skipChunk br "SCPT" // Skip scripts
        skipChunk br "SHDR" // Skip shaders
        skipChunk br "FONT" // Skip fonts
        skipChunk br "TMLN" // Skip timelines
        skipChunk br "OBJT" // Skip objects
        skipChunk br "ROOM" // Skip rooms
        skipChunk br "DAFL" // Empty
        skipChunk br "TPAG" // Skip textures

        let code : Script[] =
            readCode br

        // Read a reference map for variables...
        let variableMap : Map<ReferenceAddress, StringAddress> =
            readReferenceMap br "VARI"

        // And for functions...
        let functionMap : Map<ReferenceAddress, StringAddress> =
            readReferenceMap br "FUNC"
        
        // And merge them.
        let referenceMap =
            Utility.forceMerge variableMap functionMap

        // Extract two maps from the string chunk.
        let (stringIndexMap : Map<uint32, String>, stringAddressMap : Map<StringAddress, String>) =
            readStrings br

        skipChunk br "TXTR"
        skipChunk br "AUDO"

        // We should have hit the EOF here.
        assert (br.Read() = -1)

        {Code = code;
         ReferenceMap = referenceMap;
         StringIndexMap = stringIndexMap;
         StringAddressMap = stringAddressMap}