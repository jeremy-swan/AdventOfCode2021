open System
open System.IO

type Command = Forward of int | Up of int | Down of int
let toCommand direction distance =
    match direction with
    | "forward" -> Forward distance
    | "up" -> Up distance
    | _ -> Down distance
let buildCommand (str : string) =
    let split = str.Split(' ')
    let dir = split.[0]
    let dis = split.[1]
    dis
    |> Int32.Parse
    |> toCommand dir

let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> Seq.map buildCommand

let prepare = readFile >> parseFile

type State = 
    { Horizontal : int
      Depth : int }
module State =
    let empty = { Horizontal = 0; Depth = 0 }
let part1 () =
    prepare "input.txt"
    |> Seq.fold (fun state -> function
                              | Forward x -> { state with Horizontal = state.Horizontal + x }
                              | Up x -> { state with Depth = state.Depth - x }
                              | Down x -> { state with Depth = state.Depth + x }) State.empty
    |> fun x -> printfn "H: %i, D: %i, Product: %i" x.Horizontal x.Depth (x.Horizontal * x.Depth)

type Part2State =
    { Horizontal : int
      Depth : int
      Aim : int }
module Part2State =
    let empty = { Horizontal = 0; Depth = 0; Aim = 0 }
let part2 () =
    prepare "input.txt"
    |> Seq.fold (fun (state : Part2State) -> function
                              | Forward x -> { state with Horizontal = state.Horizontal + x; Depth = state.Depth + (x * state.Aim) }
                              | Up x -> { state with Aim = state.Aim - x }
                              | Down x -> { state with Aim = state.Aim + x }) Part2State.empty
    |> fun x -> printfn "H: %i, D: %i, A: %i, Product: %i" x.Horizontal x.Depth x.Aim (x.Horizontal * x.Depth)

[<EntryPoint>]
let main argv =
    part1 ()
    part2 ()
    0