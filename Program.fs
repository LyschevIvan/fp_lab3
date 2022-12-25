module Program =
    open Lab3

    [<EntryPoint>]
    let main (args: _[]) =
        if args.Length < 4 then
            printfn "Usage: fp_lab3  begin_x end_x point_count func_ids[]"
        else
            let a = double args[0]
            let b = double args[1]
            let n = int args[2]
            let funcIds = args[3..] |> Array.map int
            processFuncs funcIds a b n

        0
