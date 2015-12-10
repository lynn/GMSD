namespace libgmsd

module Utility =
    /// Merge two Map<'k, 'v> objects, using a given function that handles collisions.
    let mergeWith (f : 'v -> 'v -> 'v) (a : Map<'k, 'v>) (b : Map<'k, 'v>) =
        let g acc k vb =
            let w = match Map.tryFind k acc with
                    | Some va -> f va vb
                    | None -> vb
            Map.add k w acc
        Map.fold g a b

    /// Merge two Map<'k, 'v> objects, throwing an error when there is a collision.
    let forceMerge (a : Map<'k, 'v>) (b : Map<'k, 'v>) : Map<'k, 'v> =
        mergeWith (fun _ _ -> failwith "forceMerge collision") a b

    /// Find a fixpoint of f by repeatedly applying it to a starting value x.
    let rec untilEqual (f : 'X -> 'X) (x : 'X) =
        let x' = f x
        if x = x' then x else untilEqual f x'