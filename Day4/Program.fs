open System
open System.IO

type Board = Board of int list
module Board =
    let columns (Board board) = 
        seq {
            for i in {0..4} do
                yield board
                      |> List.chunkBySize 5
                      |> List.map (fun x -> x.[i])
        } |> List.ofSeq

    let rows (Board board) = board |> List.chunkBySize 5

    let hasBingo board =
        (columns board @ rows board)
        |> List.tryFind (fun l -> not (List.exists (fun x -> x >= 0) l))
        |> Option.isSome

    let markNumber (number : int) (Board board) =
        board
        |> List.map (fun x -> if x = number then -1 else x)
        |> Board

    let score (Board board) =
        board
        |> List.filter (fun x -> x >= 0)
        |> List.sum


let readFile path = File.ReadAllText path
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq

let prepare = readFile >> parseFile

let parseDrawSequence (str : string) =
    str.Split(',')
    |> Seq.map Int32.Parse


let rec playTurnPart1 (boards : Board list) (drawSequence : int list) turn =
    if turn >= List.length drawSequence then -1 else
    let newState = List.map (Board.markNumber drawSequence.[turn]) boards
    List.tryFind (Board.hasBingo) newState
    |> function
       | Some winner -> drawSequence.[turn] * Board.score winner
       | None -> playTurnPart1 newState drawSequence (turn + 1)

let rec playTurnPart2 (boards : Board list) (drawSequence : int list) turn =
    if turn >= List.length drawSequence then -1 else
    let newState = List.map (Board.markNumber drawSequence.[turn]) boards
    
    List.filter Board.hasBingo newState
    |> fun winners -> if List.length boards = 1 && List.length winners = 1 then 
                        (drawSequence.[turn] * Board.score winners.Head) 
                      else
                      playTurnPart2 (List.except winners newState) drawSequence (turn + 1)


let playGame evaluator = 
    let file = prepare "input.txt"
    let drawSequence = file.Head |> parseDrawSequence |> List.ofSeq
    
    let boards = 
        file
        |> List.skip 2
        |> List.chunkBySize 6
        |> List.map (List.take 5
                     >> List.collect 
                        (fun str -> str.Split(' ')
                                    |> Seq.filter (String.IsNullOrEmpty >> not)
                                    |> Seq.map (fun x -> Int32.Parse(x))
                                    |> List.ofSeq)
                     >> Board)

    evaluator boards drawSequence 0

let part1 () =
    playGame playTurnPart1
    |> printfn "%i"

let part2 () =
    playGame playTurnPart2
    |> printfn "%i"



[<EntryPoint>]
let main argv =

    // part1 ()
    part2 ()

    0