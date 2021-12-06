open System
open System.IO

let easySeq _start _end =
    if _start > _end then { _end.._start } else { _start.._end }

let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq

let prepare = readFile >> parseFile

type Point = 
    { X : int
      Y : int }
module Point =
    let parse (str : string) =
        let split = str.Split(',')
        { X = int split.[0]
          Y = int split.[1] }

type Line = 
    | Horizontal of Point * Point
    | Vertical of Point * Point
    | Diagonal of Point * Point


let parseLine (line : string) =
    let points = line.Split(" -> ")
                 |> List.ofSeq
                 |> List.map Point.parse

    match points.[0], points.[1] with
    | { X = x1; Y = _ }, { X = x2; Y = _ } when x1 = x2 -> 
        Some (Vertical (points.[0], points.[1]))
    | { X = _; Y = y1 }, { X = _; Y = y2 } when y1 = y2 ->
        Some (Horizontal (points.[0], points.[1]))
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when abs(y2 - y1) = abs(x2 - x1) ->
        Some (Diagonal (points.[0], points.[1]))
    | _ -> 
        None


let part1 () =
    let lines = prepare "input.txt"
                |> List.choose parseLine

    let getPoints (line : Line) =
        match line with
        | Horizontal ({ X = _start; Y = _row }, { X = _end; Y = _ }) -> 
            Seq.map (fun x -> (x, _row)) (easySeq _start _end)
        | Vertical ({ X = _col; Y = _start }, { X = _; Y = _end }) -> 
            Seq.map (fun y -> (_col, y)) (easySeq _start _end)
        | _ -> Seq.empty

    lines
    |> Seq.collect getPoints
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count > 1)
    |> Seq.length
    |> printfn "%i"


let part2 () =
    let lines = prepare "input.txt"
                |> List.choose parseLine

    let getPoints (line : Line) =
        match line with
        | Horizontal ({ X = _start; Y = _row }, { X = _end; Y = _ }) -> 
            Seq.map (fun x -> (x, _row)) (easySeq _start _end)
        | Vertical ({ X = _col; Y = _start }, { X = _; Y = _end }) -> 
            Seq.map (fun y -> (_col, y)) (easySeq _start _end)
        | Diagonal ({ X = x1; Y = y1 }, { X = x2; Y = y2 }) when sign(y2 - y1) = sign(x2 - x1) -> 
            Seq.zip (easySeq x1 x2) (easySeq y1 y2)
        | Diagonal ({ X = x1; Y = y1 }, { X = x2; Y = y2 }) -> 
            Seq.rev (easySeq y1 y2)
            |> Seq.zip (easySeq x1 x2)

    lines
    |> Seq.collect getPoints
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count > 1)
    |> Seq.length
    |> printfn "%i"
    

[<EntryPoint>]
let main argv =

    part1 ()
    part2 ()

    0 