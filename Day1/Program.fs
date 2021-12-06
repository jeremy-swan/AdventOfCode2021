open System
open System.IO

let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> Seq.map (fun x -> Int32.Parse(x))

let prepare = readFile >> parseFile

let part1 () = 
    prepare "input.txt"
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> x < y)
    |> Seq.length
    |> printfn "%i"


let slidingWindow windowSize (sequence : int seq) =
    seq {
        for i in 0..((Seq.length sequence) - windowSize) do
            yield sequence
                  |> Seq.skip i
                  |> Seq.take windowSize
                  |> Seq.sum
    }

let part2 () =
    prepare "input.txt"
    |> slidingWindow 3
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> x < y)
    |> Seq.length
    |> printfn "%i"


[<EntryPoint>]
let main argv =

    part1 ()
    part2 ()

    0
