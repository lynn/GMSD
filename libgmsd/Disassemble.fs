namespace libgmsd

type BitConverter = System.BitConverter

/// Functions related to disassembling the bytecode in a `Word []` into a list
/// of VM instructions: `Disassemble.disassemble` is the most important
/// function defined here.

module Disassemble =
    /// A word in the VM bytecode.
    type Word = uint32
    
    /// Used internally by the bytecode parser.
    type InstructionParseState =
        | NextInstruction
        | AfterPop of Word
        | AfterPushDoubleFirst
        | AfterPushDoubleSecond of Word
        | AfterPushInt64First
        | AfterPushInt64Second of Word
        | AfterPushFloat
        | AfterPushInt32
        | AfterPushBoolean
        | AfterPushInt24
        | AfterPushString
        | AfterPushVariable of Word
        | AfterCall of Word

    type DoubleOp =
        | Con  // Conversion ((T)b)
        | Mul  // Multiplication (a * b)
        | Div  // Division (a / b)
        | Idv  // Integer division (a div b)
        | Rem  // Remainder (a % b)
        | Add  // Addition (a + b)
        | Sub  // Subtraction (a - b)
        | And  // Bitwise AND (a & b)
        | Or   // Bitwise OR (a | b)
        | Xor  // Bitwise XOR (a ^ b)
        | Neg  // Negation (-b)
        | Not  // Bitwise/logical NOT (!b)
        | Shl  // Shift left (a << b)
        | Shr  // Shift right (a >> b)
        | Slt  // Less than (a < b)
        | Sle  // Less than or equal (a <= b)
        | Seq  // Equal (a == b)
        | Sne  // Not equal (a != b)
        | Sge  // Greater than or equal (a >= b)
        | Sgt  // Greater than (a > b)

    let tryReadDoubleOp : Word -> DoubleOp option =
        function
        | 0x03u -> Some Con
        | 0x04u -> Some Mul
        | 0x05u -> Some Div
        | 0x06u -> Some Idv
        | 0x07u -> Some Rem
        | 0x08u -> Some Add
        | 0x09u -> Some Sub
        | 0x0au -> Some And
        | 0x0bu -> Some Or 
        | 0x0cu -> Some Xor
        | 0x0du -> Some Neg
        | 0x0eu -> Some Not
        | 0x0fu -> Some Shl
        | 0x10u -> Some Shr
        | 0x11u -> Some Slt
        | 0x12u -> Some Sle
        | 0x13u -> Some Seq
        | 0x14u -> Some Sne
        | 0x15u -> Some Sge
        | 0x16u -> Some Sgt
        | _    -> None

    type SingleOp =
        | Dup    // Duplicate the top value on the stack.
        | Ret    // Pop a value and return from this call.
        | Exit   // Return 0 from this call.
        | Popz   // Pop a value from the stack and discard it.

    let tryReadSingleOp : Word -> SingleOp option =
        function
        | 0x82u -> Some Dup
        | 0x9du -> Some Ret
        | 0x9eu -> Some Exit
        | 0x9fu -> Some Popz
        | _    -> None

    /// Data types used by the VM.
    type DataType =
        | Double | Float | Int32 | Int64 | Boolean
        | Variable | String | Instance | Int16

    let readDataType : Word -> DataType =
        function
        | 0x00u -> Double
        | 0x01u -> Float
        | 0x02u -> Int32
        | 0x03u -> Int64
        | 0x04u -> Boolean
        | 0x05u -> Variable
        | 0x06u -> String
        | 0x07u -> Instance
        | 0x0fu -> Int16
        | w     -> failwith (sprintf "Unknown data type %d" w)

    let readDataTypePair (w : Word) : DataType * DataType =
        (readDataType (w &&& 0x0Fu), readDataType ((w &&& 0xF0u) >>> 4))

    type InstanceType =
        | StackTopOrGlobal
        | Self | Other | All | Noone | Global
        | ObjectSpecific of int16

    let showInstanceType : InstanceType -> string =
        function
        | StackTopOrGlobal -> ""
        | x -> sprintf "%A" x

    let readInstanceType : int16 -> InstanceType =
        function
        | -0s -> StackTopOrGlobal
        | -1s -> Self
        | -2s -> Other
        | -3s -> All
        | -4s -> Noone
        | -5s -> Global
        | w   -> ObjectSpecific(w)

    /// The condition type in a branch instruction.
    type BranchConditionality =
        | Unconditional | IfTrue | IfFalse
        | PushEnv | PopEnv

    let tryReadBranch : Word -> BranchConditionality option =
        function
        | 0xb7u -> Some Unconditional
        | 0xb8u -> Some IfTrue
        | 0xb9u -> Some IfFalse
        | 0xbbu -> Some PushEnv
        | 0xbcu -> Some PopEnv
        | _     -> None

    /// The type of a variable reference.
    type VariableType =
        | Array | StackTop | Normal

    let readVariableType : Word -> VariableType =
        function
        | 0x00u -> Array
        | 0x80u -> StackTop
        | 0xa0u -> Normal
        | w     -> failwith (sprintf "Unkown variable type %d" w)

    let showVariableType : VariableType -> string =
        function
        | Array    -> "[]"
        | StackTop -> "*"
        | Normal   -> ""

    /// A variable reference.
    type Reference =
        {Position: ReferenceAddress;
         Type: VariableType;
         NextOccurrenceOffset: Word}

    let readReference (w : Word) (addr : ReferenceAddress) : Reference =
        {Position = addr;
         Type = readVariableType (w >>> 24);
         NextOccurrenceOffset = (w &&& 0x00FFFFFFu)}

    let showReference (form : Unpack.Form) (r : Reference) =
            form.StringAddressMap.[form.ReferenceMap.[r.Position]] + showVariableType r.Type
        
    /// A VM instruction.
    type Instruction =
        | DoubleTypeInstruction of DoubleOp * (DataType * DataType)
        | SingleTypeInstruction of SingleOp * DataType
        | BranchInstruction of CodeAddress * BranchConditionality
        | PopInstruction of InstanceType * (DataType * DataType) * Reference
        | PushDoubleInstruction of double
        | PushInt64Instruction of int64
        | PushFloatInstruction of single
        | PushInt32Instruction of int32
        | PushBooleanInstruction of bool
        | PushInt24Instruction of int32
        | PushStringInstruction of uint32
        | PushInt16Instruction of int16
        | PushVariableInstruction of InstanceType * Reference
        | CallInstruction of DataType * int * Reference
        | BreakInstruction of int16
        | UnknownInstruction of Word

    let showInstruction (form : Unpack.Form) : Instruction -> string =
        let showRef = showReference form
        function
        | DoubleTypeInstruction(op, _)    -> sprintf "%A\n" op
        | SingleTypeInstruction(op, _)    -> sprintf "%A\n" op
        | BranchInstruction(addr, bc)     -> sprintf "%A goto %08x\n" bc addr
        | PopInstruction(it, _, r)        -> sprintf "%A.%s = pop\n" it (showRef r)
        | PushDoubleInstruction(x)        -> sprintf "push %A; " x
        | PushInt64Instruction(x)         -> sprintf "push %A; " x
        | PushFloatInstruction(x)         -> sprintf "push %A; " x
        | PushInt32Instruction(x)         -> sprintf "push %A; " x
        | PushBooleanInstruction(x)       -> sprintf "push %A; " x
        | PushInt24Instruction(x)         -> sprintf "push %A; " x
        | PushStringInstruction(x)        -> sprintf "push %A; " form.StringIndexMap.[x]
        | PushInt16Instruction(x)         -> sprintf "push %A; " x
        | PushVariableInstruction(it, r)  -> sprintf "push %A.%s; " it (showRef r)
        | CallInstruction(_, n, r)        -> sprintf "call[%d] %s\n" n (showRef r)
        | BreakInstruction(s)             -> sprintf "break %A\n" s
        | UnknownInstruction(w)           -> sprintf "<%08x>\n" w

    /// A VM instruction, tagged with its position.
    type PosInstruction =
        {Position: CodeAddress;
         Instruction: Instruction}
    
    let showPosInstruction (form : Unpack.Form) (t : PosInstruction) : string =
        sprintf "[%08x] %s" t.Position (showInstruction form t.Instruction)

    /// The function `disassemble` folds with.
    let disassembleStep (state : InstructionParseState * PosInstruction list)
                        (w : Word, addr : CodeAddress)
                        : (InstructionParseState * PosInstruction list) =
        
        /// Some helper functions:
        /// Tag an instruction with the address seen n blocks ago.
        let ago (n : int) : Instruction -> PosInstruction =
            fun x -> {Position = addr - uint32 (4 * n); Instruction = x}
        
        /// Tag an instruction with the current address.
        let here : Instruction -> PosInstruction = ago 0
     
        // Decode the current word.
        match state with
        | (NextInstruction, xs) ->
            let msb = (w &&& 0xFF000000u) >>> 24
            match tryReadDoubleOp msb with
            | Some op ->
                let types = readDataTypePair ((w &&& 0x00FFFF00u) >>> 8)
                let x = DoubleTypeInstruction(op, types)
                (NextInstruction, here x :: xs)
            | None ->
                match tryReadSingleOp msb with
                | Some op ->
                    let typ = readDataType ((w &&& 0x00FF0000u) >>> 16)
                    let x = SingleTypeInstruction(op, typ)
                    (NextInstruction, here x :: xs)
                | None ->
                    match tryReadBranch msb with
                    | Some b ->
                        let offset = w &&& 0x00FFFFFFu
                        let goal =
                            if (offset &&& 0x00800000u = 0u) then
                                addr + 4u * offset
                            else
                                addr + 4u * (offset - 0x01000000u)
                        (NextInstruction, here (BranchInstruction(goal, b)) :: xs)
                    | None ->
                        match msb with
                        | 0x41u -> (AfterPop w, xs)
                        | 0xc0u -> // Push
                            match readDataType ((w &&& 0x00FF0000u) >>> 16) with
                            | Double   -> (AfterPushDoubleFirst, xs)
                            | Float    -> (AfterPushFloat, xs)
                            | Int32    -> (AfterPushInt32, xs)
                            | Int64    -> (AfterPushInt64First, xs)
                            | Boolean  -> (AfterPushBoolean, xs)
                            | Variable -> (AfterPushVariable(w), xs)
                            | String   -> (AfterPushString, xs)
                            | Int16    ->
                                let x = PushInt16Instruction(int16 (w &&& 0x0000FFFFu))
                                (NextInstruction, here x :: xs)
                            | Instance -> failwith "Cannot push Instance type."
                            
                        | 0xdau -> (AfterCall(w), xs)
                        | 0xffu ->
                            let x = BreakInstruction(int16 (w &&& 0x0000FFFFu))
                            (NextInstruction, here x :: xs)
                        | i -> failwith (sprintf "Invalid instruction %08x" i)
        | (AfterPop w1, xs) ->
            let it = readInstanceType (int16 (w1 &&& 0x0000FFFFu))
            let types = readDataTypePair ((w1 &&& 0x00FF0000u) >>> 16)
            let dest = readReference w (addr - 4u)
            let x = PopInstruction(it, types, dest)
            (NextInstruction, ago 1 x :: xs)
        | (AfterPushDoubleFirst, xs) -> (AfterPushDoubleSecond w, xs)
        | (AfterPushDoubleSecond w1, xs) ->
            let b1 = BitConverter.GetBytes(w1)
            let b2 = BitConverter.GetBytes(w)
            let p = BitConverter.ToDouble(Array.append b1 b2, 0)
            (NextInstruction, ago 2 (PushDoubleInstruction p) :: xs)
        | (AfterPushInt64First, xs) -> (AfterPushInt64Second w, xs)
        | (AfterPushInt64Second w1, xs) ->
            let b1 = BitConverter.GetBytes(w1)
            let b2 = BitConverter.GetBytes(w)
            let p = BitConverter.ToInt64(Array.append b1 b2, 0)
            (NextInstruction, ago 2 (PushInt64Instruction p) :: xs)
        | (AfterPushFloat, xs) ->
            let b = BitConverter.GetBytes(w)
            let p = BitConverter.ToSingle(b, 0)
            (NextInstruction, ago 1 (PushFloatInstruction p) :: xs)
        | (AfterPushInt32, xs) ->
            let x = PushInt32Instruction(int32 w)
            (NextInstruction, ago 1 x :: xs)
        | (AfterPushBoolean, xs) ->
            let x = PushBooleanInstruction(w = 1u)
            (NextInstruction, ago 1 x :: xs)
        | (AfterPushInt24, xs) ->
            let x = PushInt24Instruction(int32 w)
            (NextInstruction, ago 1 x :: xs)
        | (AfterPushString, xs) ->
            let x = PushStringInstruction(w)
            (NextInstruction, ago 1 x :: xs)
        | (AfterPushVariable w1, xs) ->
            let it = readInstanceType (int16 (w1 &&& 0x0000FFFFu))
            let ref = readReference w (addr - 4u)
            let x = PushVariableInstruction(it, ref)
            (NextInstruction, ago 1 x :: xs)
        | (AfterCall w1, xs) ->
            let returnType = readDataType ((w1 &&& 0x00FF0000u) >>> 16)
            let argumentCount = int (w1 &&& 0x0000FFFFu)
            let ref = readReference w (addr - 4u)
            let x = CallInstruction(returnType, argumentCount, ref)
            (NextInstruction, ago 1 x :: xs)
    
    /// Dissassemble an array of words into bytecode instructions, and tag
    /// them with their addresses, starting at a given address.
    let disassemble (start : uint32) (ws : Word []) : PosInstruction list =
        let ws' = Array.mapi (fun i x -> (x, uint32 i * 4u + start)) ws
        match Array.fold disassembleStep (NextInstruction, []) ws' with
        | (NextInstruction, code) -> List.rev code
        | _ -> failwith "Unterminated opcode!"