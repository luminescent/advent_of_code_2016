module Day24

open System 
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Collections


let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let prettyPrintFromFile() = 
    let lines = 
        File.ReadAllLines("Day24.txt")
        |> Array.map (fun s -> s.Replace(".", " "))
        |> Array.toList 
    lines
    |> List.iter (fun s -> printfn "%s" s)

let parseGrid() = 
    let lines = 
        File.ReadAllLines("Day24.txt")
        |> Array.map (fun s -> s.Replace(".", " "))
    let grid = 
        Array2D.init lines.[0].Length lines.Length (fun a b -> lines.[b].[a])
    grid 
    

let prettyPrint (grid: char[,]) = 
    [0..(grid.GetLength(1) - 1)]
    |> List.iter (fun y -> 
                    printfn ""
                    [0..(grid.GetLength(0) - 1)]
                    |> List.iter (fun x -> printf "%s" (grid.[x, y].ToString())) )
    

// not walls 
let getNeighbours (grid: char[,]) x y = 
    let possibles = 
        [(0, -1); (0, 1); (-1, 0); (1, 0)]
        |> List.map (fun (a, b) -> (a + x, b + y))
        |> List.filter (fun (a, b) -> a >= 0 && a < grid.GetLength(0) && b >= 0 && b < grid.GetLength(1))
        |> List.filter (fun (a, b) -> grid.[a, b] <> '#')
    possibles


let findDeadEnds (grid: char[,]) = 
    let isDeadEnd grid x y = ((getNeighbours grid x y) |> List.length) <= 1 
    let mutable deadEnds = List.empty
    for x in 0..(grid.GetLength(0) - 1) do 
        for y in 0..(grid.GetLength(1) - 1) do 
            if (grid.[x, y] = ' ') then 
                let neighbours = getNeighbours grid x y 
                if (neighbours.Length <= 1) then 
                    deadEnds <- (x, y) :: deadEnds
    deadEnds 
                         

let rec fillDeadEnds grid =
    let deadEnds = findDeadEnds grid 
    match deadEnds.Length with 
    | 0 -> ()
    | _ -> 
        deadEnds
        |> List.iter(fun (x, y) -> grid.[x, y] <- '#')
        fillDeadEnds grid 

type State = 
            { 
                Coordinates: int*int 
                Path: (int*int) list 
            }
     
let shortestPath grid startX startY endX endY = 
    let q = new Queue<State>()
    q.Enqueue( { Coordinates = (startX, startY); Path = [] })
    let visited = new HashSet<int*int>()
    //visited.Add((startX, startY))
    let mutable minLength = 10000

    while q.Count > 0 do 
        let next = q.Dequeue()
        if next.Coordinates = (endX, endY) then 
            // printfn "Found a solution with length %i" next.Path.Length
            if (next.Path.Length) < minLength then 
                minLength <- next.Path.Length
        else 
            let neighbours = getNeighbours grid (fst next.Coordinates) (snd next.Coordinates)
            neighbours
            |> List.filter(fun n -> (not (visited.Contains(n))))
            |> List.iter (fun n -> q.Enqueue({Coordinates = (fst n, snd n); Path = (fst n, snd n) :: next.Path }))
            neighbours
            |> List.iter (fun n -> visited.Add(n) |> ignore)

    minLength 


let nodeShortestPath grid startNode endNode = 
    //printfn "Computing path from %A to %A" startNode endNode
    let (startX, startY) = startNode 
    let (endX, endY) = endNode 
    shortestPath grid startX startY endX endY 

let tryFindElement element (grid: 'a[,]) = 
    let rec tryFE (grid: 'a[,]) x y element = 
        match grid.[x, y] = element with 
        | true -> Some(x, y)
        | _ -> 
            let newX, newY = 
                match (x, y) with 
                | (a, b) when a < grid.GetLength(0) - 1 -> (a + 1, b)
                | (a, b) when a = grid.GetLength(0) - 1 -> (0, b + 1)
            match newY = grid.GetLength(1) with 
            | true -> None
            | _ -> tryFE grid newX newY element 
                    
    tryFE grid 0 0 element 
           
let permutationToPathLength grid path = 
    let minPath = 
        path 
        |> List.map (fun c -> (tryFindElement (Char.Parse(c.ToString())) grid).Value )
        |> List.toSeq 
        |> Seq.windowed 2 
        |> Seq.map (fun s -> s |> Seq.toList)
        |> Seq.map (fun window -> (window.[0], window.[1]))
        |> Seq.fold (fun acc (startNode, endNode) -> acc + (nodeShortestPath grid startNode endNode)) 0 
    printfn "Min path for %A: %i" path minPath
    (path, minPath)

let findGlobalMin grid = 
    let pathFragments = 
        [0..7]
        |> List.map (fun c -> (tryFindElement (Char.Parse(c.ToString())) grid).Value ) 
        |> combinations 2
        |> List.collect (fun pair -> [pair; pair |> List.rev])
        |> List.map (fun pair -> ((pair.[0], pair.[1]), nodeShortestPath grid pair.[0] pair.[1]))
        |> Map.ofList 
    let computePathLength (pathFragments: Map<((int*int)*(int*int)), int>) path = 
        let minPath = 
            path 
            |> List.map (fun c -> (tryFindElement (Char.Parse(c.ToString())) grid).Value )
            |> List.toSeq 
            |> Seq.windowed 2 
            |> Seq.map (fun s -> s |> Seq.toList)
            |> Seq.map (fun window -> (window.[0], window.[1]))
            |> Seq.fold (fun acc (startNode, endNode) -> acc + (pathFragments.[(startNode, endNode)])) 0 
        minPath        
        
    let globalMin = 
        [1..7]
        |> permute 
        |> List.map (fun l -> 0 :: l)
        |> List.map (fun l -> [0] |> List.append l)
        |> List.map (computePathLength pathFragments)
        |> List.min
    globalMin

let run_day24() = 

    let grid = parseGrid()
    fillDeadEnds grid 
    
    //prettyPrint grid 

    let globalMin = 
        findGlobalMin grid 

    printfn "Min path: %i" globalMin

