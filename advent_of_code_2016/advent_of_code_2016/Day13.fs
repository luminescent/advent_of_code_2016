module Day13

open System
open System.Collections.Generic

type Vertex = int * int 
type Graph = Set<Vertex> 
type Distances = Dictionary<Vertex, int>
type Path = Dictionary<Vertex, Vertex>
type VertexSet = HashSet<Vertex>


let count x = Seq.filter ((=) x) >> Seq.length

let createMatrix width height seed = 
    let createColumn height column seed = 
        [0..(height - 1)]
        |> List.map (fun y -> seed + (column + y) * (column + y) + 3 * column + y)
        |> List.map (fun i -> Convert.ToString(i, 2))
        |> List.map (count '1')
        |> List.map (fun i -> i % 2)
        
    [0..(width - 1)]
    |> List.map (fun column -> createColumn height column seed)

let prettyPrint (matrix: 'a list list) width height = 
    [0..(height - 1)]
    |> List.iter (fun y -> 
                    printfn ""
                    [0..(width - 1)]
                    |> List.iter (fun x -> printf "%A " matrix.[x].[y]))

let findKeyWithMinValue (dictionary: Dictionary<'a, 'b>) (vertexSet: HashSet<'a>) = 
    seq {for k in dictionary.Keys -> k} 
    |> Seq.filter vertexSet.Contains 
    |> Seq.minBy (fun x -> dictionary.[x])    

let getNeighbours vertex graph (vertexSet: HashSet<int*int>) = 
    let (x, y) = vertex 
    [(x - 1, y); (x, y - 1); (x, y + 1); (x + 1, y)]
    |> Set.ofList 
    |> Set.intersect graph 
    |> Set.filter vertexSet.Contains 


let dijkstra (matrix: int list list) (start: Vertex) = 
    let graph =
        [0..(matrix.Length - 1)]
        |> List.collect (fun x -> [0..(matrix.[x].Length - 1)] 
                                    |> List.filter (fun y -> matrix.[x].[y] = 0) 
                                    |> List.map (fun y -> (x, y)))
        |> Set.ofList

    let distances = new Distances()
    graph
    |> Set.iter (fun v -> distances.Add(v, 10000))
    distances.[start] <- 0

    let path = new Path()

    let vertexSet = new VertexSet()
    graph
    |> Set.iter(fun v -> vertexSet.Add(v) |> ignore)


    while vertexSet.Count > 0 do 
        let vertexWithMinDistance = findKeyWithMinValue distances vertexSet 
        
        (vertexSet.Remove vertexWithMinDistance) |> ignore 

        let neighbours = getNeighbours vertexWithMinDistance graph vertexSet
        neighbours
        |> Set.iter(fun neighbour -> 
                        let neighbourDistance = distances.[vertexWithMinDistance] + 1
                        if neighbourDistance < distances.[neighbour] then 
                            distances.[neighbour] <- neighbourDistance
                            path.[neighbour] <- vertexWithMinDistance )

    distances


let run_day13() = 
    
    let matrix = createMatrix 10 7 10 
    prettyPrint matrix 10 7

    let distances = dijkstra matrix (1, 1)
    printfn "Min path length: %i" distances.[(7, 4)]

    let matrix = createMatrix 50 50 1364 
    let distances = dijkstra matrix (1, 1) 
    printfn "Min path length: %i" distances.[(31, 39)]

    printfn 
        "Distinct vertexes reachable in at most 50 steps: %A"
        (seq { for v in distances.Values -> v}
         |> Seq.filter (fun v -> v <= 50)
         |> Seq.length)

     
   

