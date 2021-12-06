open System
open System.IO

module State =
    let empty = Array.zeroCreate<int> >> Seq.ofArray
    
let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq

let prepare = readFile >> parseFile

let flipString (str : string) =
    str
    |> Seq.map ((fun x -> if x = '1' then '0' else '1') >> string)
    |> String.concat ""

let mostCommonBits (lines : string list) =
    lines
    |> List.fold 
        (fun state line -> line
                           |> Seq.map (fun ch -> if ch = '0' then 0 else 1)
                           |> Seq.zip state
                           |> Seq.map (fun (a, b) -> a + b))
        (State.empty lines.[0].Length)
    |> List.ofSeq
    |> List.map (fun x -> if (float x) >= (float lines.Length / 2.) then "1" else "0")
    |> List.fold (+) ""


let part1 () =
    let gammaRateString = prepare "input.txt"
                          |> mostCommonBits

    let gammaRate = Convert.ToUInt32(gammaRateString, 2)
    let epsilonRate = Convert.ToUInt32(flipString gammaRateString, 2)

    printfn "GammaString: %s, Gamma: %i, Epsilon: %i, Product: %i, Expected: %s" gammaRateString gammaRate epsilonRate (gammaRate * epsilonRate) "2035764"


let part2 () =
    let lines = prepare "input.txt"

    let rec filterLines (eval : string list -> string) lines i =
        let comparer = lines |> eval
        let out = List.filter (fun (line : string) -> line.[i] = comparer.[i]) lines

        match List.length out with 
        | 1 -> List.head out
        | _ -> filterLines eval out (i + 1)

    let oxygen = filterLines mostCommonBits lines 0 |> fun x -> Convert.ToInt32(x, 2)
    let co2 = filterLines (mostCommonBits >> flipString) lines 0 |> fun x -> Convert.ToInt32(x, 2)

    printfn "Oxygen: %i, CO2: %i, Product: %i" oxygen co2 (oxygen * co2)


[<EntryPoint>]
let main _ =

    part1 ()
    part2 ()
    0