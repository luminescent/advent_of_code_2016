module Day15

open System 
open System.IO

type Disc = { Positions: int; Start: int; Index: int }

let parseDisc (s: string) = 
    let parts = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    { 
        Index = parts.[1].Substring(1) |> Int32.Parse; 
        Positions = parts.[3] |> Int32.Parse; 
        Start = parts.[11].Substring(0, parts.[11].Length - 1) |> Int32.Parse 
    }

let positionWhenReached buttonPushedTime disc = 
    let discPositionAtButtonPushedTime = (disc.Start + buttonPushedTime) % disc.Positions 
    let discPositionWhenReached = (discPositionAtButtonPushedTime + disc.Index) % disc.Positions 
    discPositionWhenReached

let findFirstTimeWhenAllAreReachedAt0 discs = 
    let rec iterateUntilAllReach0 startTime discs = 
        let allPositions0 = 
            discs
            |> Array.map (positionWhenReached startTime)
            |> Array.forall (fun x -> x = 0)
        match allPositions0 with 
        | true -> startTime 
        | false -> iterateUntilAllReach0 (startTime + 1) discs

    iterateUntilAllReach0 0 discs 

let run_day15() =
    let discs = 
        File.ReadAllLines("Day15.txt")
        |> Array.map parseDisc

//    printfn "Disk #1 when button pushed t = %i: %i" 5 (positionWhenReached 5 discs.[0])
//    printfn "Disk #2 when button pushed t = %i: %i" 5 (positionWhenReached 5 discs.[1])

    let ft = findFirstTimeWhenAllAreReachedAt0 discs 
    printfn "First time when the marble falls through: %i" ft
