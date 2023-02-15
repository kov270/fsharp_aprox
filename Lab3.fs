module Lab3

open System
open Microsoft.FSharp.Collections

let linear points =
    let sx = List.fold (fun state (x, _) -> state + x) 0. points
    let sxx = List.fold (fun state (x, _) -> state + x * x) 0. points
    let sy = List.fold (fun state (_, y) -> state + y) 0. points
    let sxy = List.fold (fun state (x, y) -> state + x * y) 0. points
    let n = points.Length
    let a = (sxy * (double n) - sx * sy) / (sxx * (double n) - sx * sx)
    let b = (sxx * sy - sx * sxy) / (sxx * (double n) - sx * sx)
    let f x = a * x + b
    f

let segment (points: list<double * double>) =
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

let logarifm (points: list<double * double>) =
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

let getFunc (id: int) points =
    async {
        match id with
        | 1 -> return segment points
        | 2 -> return logarifm points
        | _ -> return linear points
    }

let genPoint (n: int) points =
    match points with
    | (x2, _) :: (x1, _) :: _ ->
        let mult = (x2 - x1) / (double n)
        let getPoint (i: int) = x1 + (double i) * mult
        getPoint
    | _ -> fun _ -> 0

let print (funcs: ((double -> double) * int)[]) (pointGen: int -> double) count =
    funcs
    |> Array.map (fun (_, id) -> $"%d{id} | ")
    |> Array.fold (+) ""
    |> printfn "%s"

    [ 0 .. count - 1 ]
    |> List.map (fun i ->
        funcs
        |> Array.map (fun (f, _) -> $"x: %11.4f{pointGen i}, y: %11.4f{f (pointGen i)} | ")
        |> Array.fold (+) ""
        |> printfn "%s")
    |> ignore

