module Program =
    open Lab3

    [<EntryPoint>]
    let main (args: _[]) =
        if args.Length <> 4 && args.Length <> 5 then
            printfn "Usage: fp_lab3 func_id begin_x end_x point_count"
            printfn "Or"
            printfn "Usage: fp_lab3 func_id_1 func_id_2 begin_x end_x point_count"
        else
            if args.Length = 4 then
                let funcId = args[0]
                let a = double args[1]
                let b = double args[2]
                let n = int args[3]
                processOneFunc funcId a b n []
            else
                let funcId = args[0]
                let funcId2 = args[1]
                let a = double args[2]
                let b = double args[3]
                let n = int args[4]
                processOneFunc funcId a b n []
                printfn ("-------------------------------------")
                processOneFunc funcId2 a b n []

            ()

        0
