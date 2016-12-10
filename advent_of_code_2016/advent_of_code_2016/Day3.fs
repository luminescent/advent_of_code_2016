module Day3 

open System
open System.IO

let is_viable_triangle triangle =
    let (x, y, z) = triangle 
    x + y > z && x + z > y && z + y > x

let parse_edges (s: string) =
    s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse

let parse_horizontal_triangle (s: string) =
    let edges = parse_edges s
    (edges.[0], edges.[1], edges.[2])

let parse_vertical_triangles (line0: string) (line1: string) (line2: string) = 
    let edges0 = parse_edges line0
    let edges1 = parse_edges line1 
    let edges2 = parse_edges line2 

    [0 .. 2]
        |> List.map (fun i -> (edges0.[i], edges1.[i], edges2.[i])) 
    
            

let run_day3() = 
    let lines = File.ReadAllLines("Day3.txt")  

    let valid_triangles_count = 
        lines
        |> Array.map parse_horizontal_triangle
        |> Array.filter is_viable_triangle
        |> Array.length 
    
    printfn "Found %A viable triangles" valid_triangles_count      
    
    let vertical_triangles_count = 
        [0..3..(lines.Length - 1)]
        |> List.collect (fun i -> parse_vertical_triangles lines.[i] lines.[i + 1] lines.[i + 2])
        |> List.filter is_viable_triangle
        |> List.length 

    printfn "Found %A viable vertical triangles" vertical_triangles_count



