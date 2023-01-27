module Program =
    open Lab3

    [<EntryPoint>]
    let main (args: string[]) =
        match List.ofArray (Array.map int args) with
        | n::k::funcIds when funcIds.Length > 0 -> processFuncs funcIds n k
        | _ -> printfn "Usage: fp_lab3 point_count window func_ids[]"
        0
