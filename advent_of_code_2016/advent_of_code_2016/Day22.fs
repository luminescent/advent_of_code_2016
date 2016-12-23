module Day22

open System 
open System.IO 

type GridNode = { X: int; Y: int; Capacity: int; Used: int}

let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let parse (s: string) = 
    let parts = s.Split([| "-"; " "|], StringSplitOptions.RemoveEmptyEntries)
    { 
        X  = parts.[1].Substring(1) |> Int32.Parse; 
        Y = parts.[2].Substring(1) |> Int32.Parse; 
        Capacity = parts.[3].Substring(0, parts.[3].Length - 1) |> Int32.Parse;
        Used = parts.[4].Substring(0, parts.[4].Length - 1) |> Int32.Parse;
     }

let isViablePair nodeA nodeB = 
    nodeA.Used > 0 && (nodeA.X <> nodeB.X || nodeA.Y <> nodeB.Y) && nodeA.Used <= (nodeB.Capacity - nodeB.Used) 

let countViablePairs nodes =
    let pairs = 
        nodes 
        |> combinations 2 
        |> List.collect (fun pair -> [pair; pair |> List.rev])  // we want arrangements 
        |> List.filter (fun l -> isViablePair l.[0] l.[1])
    pairs |> List.iter (fun p -> printfn "%A" p)
    pairs |> List.length 

let percentage node = 
    node.Used * 100 / node.Capacity 

let prettyPrint nodes = 
    let maxX = 
        nodes 
        |> List.map (fun n -> n.X)
        |> List.max
    let maxY = 
        nodes 
        |> List.map (fun n -> n.Y)
        |> List.max

    printfn "Max X: %i; max Y: %i" maxX maxY

    let emptyNode = 
        nodes 
        |> List.find (fun n -> n.Used = 0)

    [0..maxY]
    |> List.iter (fun y -> 
                    printfn ""
                    [0..maxX]
                    |> List.iter (fun x -> 
                                    let node = nodes |> List.find(fun n -> n.X = x && n.Y = y)
                                    match node.Used with 
                                    | a when a = 0 -> printf " _ "
                                    | a when not (isViablePair node emptyNode) -> printf " # "
                                    | _ -> printf " . "))
    printfn ""


let computeMinNoOfSteps nodes = 
    let maxX = 
        nodes 
        |> List.map (fun n -> n.X)
        |> List.max
    let maxY = 
        nodes 
        |> List.map (fun n -> n.Y)
        |> List.max

    let emptyNode = 
        nodes 
        |> List.find (fun n -> n.Used = 0)

    emptyNode.X + (emptyNode.Y - 1) + (maxX - 1) + 1 + 1 + 5 * (maxX - 1) 
    


let run_day22() = 
    let nodes = 
        File.ReadAllLines("Day22.txt")
        |> Array.map parse 
        |> Array.toList 

    prettyPrint nodes 
    let noOfSteps = computeMinNoOfSteps nodes 
    printfn "Min no of steps: %i" noOfSteps


//    let viablePairs = countViablePairs nodes 
//    printfn "Viable pairs count: %A" viablePairs