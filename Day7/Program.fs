open System
open System.IO

let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split(',')
    |> Seq.map int

let prepare = readFile >> parseFile

let part1 () =
    let numbers = prepare "input.txt"
                  |> List.ofSeq
                  |> List.sort
    let median = numbers.[numbers.Length / 2]
    
    numbers 
    |> List.sumBy (fun x -> abs(x - median))
    |> printfn "%i"


let part2 () =
    let numbers = prepare "input.txt"
                  |> Seq.sort
    let average = numbers |> Seq.averageBy decimal |> int

    {average..(average+1)}
    |> Seq.map (fun x -> numbers |> Seq.sumBy (fun y -> Seq.sum {1..abs(x - y)}))
    |> Seq.min
    |> printfn "%i"


[<EntryPoint>]
let main argv =

    part1 ()
    part2 ()

    0