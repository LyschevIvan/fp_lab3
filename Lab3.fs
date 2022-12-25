module Lab3

open System


let rec handleInput pList =
    let line = Console.ReadLine()

    if (not (isNull line) && line <> "") then
        let data = line.Split(";")

        if data.Length >= 2 && data[1] <> "" then
            let x = double data[0]
            let y = double data[1]

            match pList with
            | [] -> [ (x, y) ]
            | _ -> (x, y) :: pList

        else
            handleInput pList
    else
        pList


let linear points : double -> double =
    let sx = List.fold (fun state (x, _) -> state + x) 0. points
    let sxx = List.fold (fun state (x, _) -> state + x * x) 0. points
    let sy = List.fold (fun state (_, y) -> state + y) 0. points
    let sxy = List.fold (fun state (x, y) -> state + x * y) 0. points
    let n = points.Length
    let a = (sxy * (double n) - sx * sy) / (sxx * (double n) - sx * sx)
    let b = (sxx * sy - sx * sxy) / (sxx * (double n) - sx * sx)
    let f x = a * x + b
    f

let segment (points: list<double * double>) : double -> double =
    let rec findBottomBorder i v =
        if i < points.Length then
            let x, _ = points[i]
            if x < v then i else findBottomBorder (i + 1) v
        else
            -1

    let rec findTopBorder i v =
        if i >= 0 then
            let x, _ = points[i]
            if x >= v then i else findTopBorder (i - 1) v
        else
            -1

    let f x =
        let top = findTopBorder (points.Length - 1) x
        let bottom = findBottomBorder 0 x

        if top = -1 then
            let _, yi = points[0]
            yi
        else if bottom = -1 then
            let _, yi = points[points.Length - 1]
            yi
        else
            let xi, yi = points[top]
            let xiPrev, yiPrev = points[bottom]
            let a = (yi - yiPrev) / (xi - xiPrev)
            let b = yi - a * xi
            a * x + b

    f

let logarifm (points: list<double * double>) : double -> double =
    let sx = List.fold (fun state (x, _) -> state + log x) 0. points
    let sxx = List.fold (fun state (x, _) -> state + (log x) * (log x)) 0. points
    let sy = List.fold (fun state (_, y) -> state + y) 0. points
    let sxy = List.fold (fun state (x, y) -> state + (log x) * y) 0. points
    let n = points.Length
    let delta = sxx * (double n) - (sx * sx)
    let delta1 = sxy * (double n) - (sx * sy)
    let delta2 = sxx * sy - (sx * sxy)
    let a = delta1 / delta
    let b = delta2 / delta
    let f x = a * log x + b
    f

let getFunc (funcId: int) points : double -> double =
    match funcId with
    | 1 -> segment points
    | 2 -> logarifm points
    | _ -> linear points
let getFuncName (funcId: int) : string =
    match funcId with
    | 1 -> "Сегментная аппроксимация"
    | 2 -> "Логарифмическая аппроксимация"
    | _ -> "Линейная аппроксимация"
let getPointGen (a: double) (b: double) (n: int) =
    let mult = (b - a) / (double n)
    let getPoint (i: int) = a + (double i) * mult
    getPoint

let printValues (funcs: ((double -> double) * int)[]) (pointGen: int -> double) count =
    funcs
    |> Array.map (fun fWithId ->
        let f, id = fWithId
        printfn $"{getFuncName id} : "
        [ 0..count ]
        |> List.map (fun i ->
            printfn $"x: %8.4f{pointGen i}, y: %8.4f{f (pointGen i)}")
        |> ignore

        printfn "———————————————————————————")
    |> ignore

let rec processFuncsRec (funcIds: int[]) a b n points (funcs: (double -> double)[]) =
    let newPoints = handleInput points

    if (newPoints <> points) then
        let funcsArr = funcIds |> Array.map (fun funcId -> getFunc funcId newPoints)
        processFuncsRec funcIds a b n newPoints funcsArr
    else if (points <> []) then
        let pointGen = getPointGen a b n
        printValues (Array.zip funcs funcIds) pointGen n

let rec processFuncs funcIds a b n =
    processFuncsRec funcIds a b n [] Array.empty<double -> double>
