module Day17

open System.Collections.Generic
open System
open System.Diagnostics.PerformanceData

type Move = 
    | D = 'D'
    | U = 'U'
    | R = 'R'
    | L = 'L'

type DoorState = 
    | Open
    | Closed

type RoomState = { Coordinates: int*int; AllowedMoves: Set<Move>; Path: Move list}

let getRoomStateSignature roomState = 
    roomState.AllowedMoves 
    |> Set.toList 
    |> List.map (fun m -> m.ToString())
    |> List.sort 
    |> List.append [roomState.Coordinates.ToString()]
    |> String.concat ";"


let computeMd5 (s: string) = 
    use md5 = System.Security.Cryptography.MD5.Create()
    s
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let getDoorState (hash: string) move = 
    let openChars = 
        ['b'; 'c'; 'd'; 'e'; 'f']
        |> Set.ofList 
    let canOpen = 
        match move with 
        | Move.U -> openChars |> Set.contains hash.[0] 
        | Move.D -> openChars |> Set.contains hash.[1]
        | Move.L -> openChars |> Set.contains hash.[2]
        | Move.R -> openChars |> Set.contains hash.[3]
        | _ -> failwithf "Unable to parse move %A" move
    match canOpen with 
    | true -> DoorState.Open
    | _ -> DoorState.Closed

let isFinalState roomState = 
    roomState.Coordinates = (3, 3)

let getCoordinates coordinates move = 
    let adjustments = 
        [(Move.U, (0, -1)); (Move.D, (0, 1)); (Move.L, (-1, 0)); (Move.R, (1, 0))]
        |> Map.ofList 
    (fst coordinates + fst adjustments.[move], snd coordinates + snd adjustments.[move])

let areValidCoordinates coordinates = 
    (0 <= fst coordinates ) && (fst coordinates <= 3) && (0 <= snd coordinates) && (snd coordinates <= 3)

let getHash passcode path = 
    computeMd5 (sprintf "%s%s" passcode (path |> List.map (fun m -> m.ToString()) |> String.concat ""))

let getAllowedMoves hash coordinates = 
    [ Move.U; Move.D; Move.L; Move.R ]
    |> List.map (fun m -> (m, getDoorState hash m))
    |> List.filter (fun (x, y) -> y = DoorState.Open)
    |> List.map fst
    |> List.map (fun m -> (m, getCoordinates coordinates m))
    |> List.filter (snd >> areValidCoordinates)
  

let getNextRoomStates roomState passcode =
    let hash = getHash passcode roomState.Path 
    let allowedMoves = getAllowedMoves hash roomState.Coordinates 
    allowedMoves
    |> List.map (fun (m , c) -> (m, c, [ m ] |> List.append roomState.Path ))
    |> List.map (fun (m, c, p) -> (m, c, p, getHash passcode p)) 
    |> List.map (fun (m, c, p, h) -> (m, c, p, (getAllowedMoves h c) |> List.map( fun (move, _) -> move) |> Set.ofList ))
    |> List.map (fun (m, c, p, am) -> { Coordinates = c; AllowedMoves = am; Path = p})


let pathToString (moves: Move list) = 
    moves 
    |> List.map (fun m -> m.ToString()) 
    |> String.concat ""

let run passcode =     
    let mutable minPath = 10000 
    let mutable bestPath = []
    let startCoordinates = (0, 0)
    let allowedMoves = 
        (getAllowedMoves (getHash passcode []) startCoordinates) 
        |> List.map( fun (move, _) -> move) |> Set.ofList
     
    let startRoomState = { Coordinates = (0, 0); AllowedMoves = allowedMoves; Path = []}

    let q = new Queue<RoomState>()
    q.Enqueue startRoomState

    while q.Count > 0 do 
        let currentState = q.Dequeue()
        if isFinalState currentState then 
            printfn "Found a solution: %A with length of path %i" currentState currentState.Path.Length 
            if currentState.Path.Length < minPath then 
                minPath <- currentState.Path.Length 
                bestPath <- currentState.Path 
        else 
            if currentState.Path.Length <= (min 100 minPath)  then      
                let nextStates = getNextRoomStates currentState passcode 
                nextStates
                |> List.iter (fun s -> q.Enqueue(s))

    bestPath 

let runMax passcode = 
    let mutable maxPath = 0 
    let mutable bestPath = []
    let startCoordinates = (0, 0)
    let allowedMoves = 
        (getAllowedMoves (getHash passcode []) startCoordinates) 
        |> List.map( fun (move, _) -> move) |> Set.ofList 
     
    let startRoomState = { Coordinates = (0, 0); AllowedMoves = allowedMoves; Path = []}

    let q = new Queue<RoomState>()
    q.Enqueue startRoomState

    while q.Count > 0 do 
        let currentState = q.Dequeue()
        if isFinalState currentState then 
            printfn "Found a solution: %A with length of path %i" currentState currentState.Path.Length 
            if currentState.Path.Length > maxPath then 
                maxPath <- currentState.Path.Length 
                bestPath <- currentState.Path 
        else 
            if currentState.Path.Length <= 2000  then // best guess...       
                let nextStates = getNextRoomStates currentState passcode 
                nextStates
                |> List.iter (fun s -> q.Enqueue(s))

    bestPath     

let run_part1() = 
    let bestPath = run "ihgpwlah" 
    printfn "Min path: %s" (pathToString bestPath)

    let bestPath = run "kglvqrro" 
    printfn "Min path: %s" (pathToString bestPath)

    let bestPath = run "ulqzkmiv" 
    printfn "Min path: %s" (pathToString bestPath)

    let bestPath = run "vwbaicqe" 
    printfn "Min path: %s" (pathToString bestPath)


let run_part2() = 
//    let bestPath = runMax "ihgpwlah" 
//    printfn "Max path length: %i" (pathToString bestPath).Length

//    let bestPath = runMax "kglvqrro" 
//    printfn "Min path: %s" (pathToString bestPath)
//
//    let bestPath = runMax "ulqzkmiv" 
//    printfn "Min path: %s" (pathToString bestPath)
//
    let stopWatch = new System.Diagnostics.Stopwatch()
    stopWatch.Start()
    let bestPath = runMax "vwbaicqe" 
    printfn "Max path length: %i" (pathToString bestPath).Length
    stopWatch.Stop()
    printfn "%A" stopWatch.Elapsed.TotalSeconds



let run_day17() = 
    
    run_part1()
    run_part2()



    


