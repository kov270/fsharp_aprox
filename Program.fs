open System
open Microsoft.FSharp.Collections

module Program =
    open Lab3

    let rec reader k (pList: (double * double) list) =
        let line = Console.ReadLine()

        if (not (isNull line) && line <> "") then
            let data = line.Split(";")

            if data.Length >= 2 && data[1] <> "" then
                let x = double data[0]
                let y = double data[1]

                match pList.Length with
                | 0 -> [ (x, y) ]
                | _ -> (x, y) :: List.truncate (k - 1) pList
            else
                reader k pList
        else
            pList

    let rec processingRec (ids: int[]) n k points =
        let newPoints = reader k points

        match newPoints with
        | pts when pts.Length = k && pts <> points ->
            let funcsArr =
                Seq.map (fun id -> (getFunc id newPoints)) ids
                |> Async.Parallel
                |> Async.RunSynchronously

            let pointGen = genPoint n newPoints
            print (Array.zip funcsArr ids) pointGen n
            processingRec ids n k newPoints
        | pts when pts <> points -> processingRec ids n k newPoints
        | _ -> ()

    let processing ids n k = processingRec (Array.ofList ids) n k []

    [<EntryPoint>]
    let main (args: string[]) =
        match List.ofArray (Array.map int args) with
        | n::k::id when id.Length > 0 -> processing id n k
        | _ -> printfn "error bad args"
        0
