module Program =
    open Lab3

    [<EntryPoint>]
    let main (args: _[]) =
        if args.Length < 2 then
            printfn "Usage: fp_lab3 point_count window func_ids[]"
        else
            let n = int args[0]
            let k = int args[1]
            let funcIds = args[2..] |> Array.map int
            processFuncs funcIds n k

        0
