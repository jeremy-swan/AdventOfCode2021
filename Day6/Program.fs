open System
open System.IO

let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split(',')
    |> Seq.map int

let prepare = readFile >> parseFile


let rec countChildren (daysRemaining : int) (delay : int) : bigint =
    let numberOfChildren = ceil(float (daysRemaining - delay) / (float 7)) |> int

    if numberOfChildren <= 0 then bigint 1 else 
    { 0..numberOfChildren }
    |> Seq.sumBy (fun i ->
        let childDaysRemaining = ((daysRemaining - delay) - (i * 7))
        countChildren childDaysRemaining 9)

let part1 () =
    prepare "input.txt"
    |> Seq.countBy id
    |> Seq.sumBy (fun (x, count) -> (countChildren 80 x) * (bigint count))
    |> printfn "%A"

let part2 () =
    prepare "input.txt"
    |> Seq.countBy id
    |> Seq.sumBy (fun (x, count) -> let res = (countChildren 256 x) * (bigint count)
                                    printfn "Done %i" x
                                    res)
    |> printfn "%A"

[<EntryPoint>]
let main argv =

    part1 ()
    part2 ()

    0