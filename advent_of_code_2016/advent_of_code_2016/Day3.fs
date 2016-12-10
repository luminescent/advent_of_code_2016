module Day3 

open System
open System.IO

let is_viable_triangle triangle =
    let (x, y, z) = triangle 
    x + y > z && x + z > y && z + y > x

let parse_horizontal_triangle (s: string) =
    let edges = 
        s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
    (edges.[0], edges.[1], edges.[2])

let run_day3() = 
    let valid_triangles_count = 
        File.ReadAllLines("Day3.txt")
        |> Array.map parse_horizontal_triangle
        |> Array.filter is_viable_triangle
        |> Array.length 
    
    printfn "Found %A viable triangles" valid_triangles_count        



