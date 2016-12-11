module Day8

open System 
open System.IO
open System.Collections.Generic
open System.Linq

// 50 * 6 
type RowRotation = { Row: int; By: int}
type ColumnRotation = { Column: int; By:int }
type Rect = {Row: int; Column:int}

type Instruction =
    | LightRectangle of Rect
    | RotateRow of RowRotation
    | RotateColumn of ColumnRotation
    
let light_rectangle instruction (panel: Dictionary<(int * int), bool>) = 
    [0..(instruction.Row - 1)]
    |> List.iter (fun row -> [0..(instruction.Column - 1)] 
                                |> List.iter (fun column -> panel.[(row, column)] <- true)) 

let rotate_row (row_rotation:RowRotation) (panel: Dictionary<(int * int), bool>) = 
    let new_row = new Dictionary<int, bool>()
    [0..49]
    |> List.iter (fun i -> new_row.Add( (i + row_rotation.By) % 50, panel.[(row_rotation.Row, i)]))
    [0..49]
    |> List.iter (fun i -> panel.[(row_rotation.Row, i)] <- new_row.[i])

let rotate_column (column_rotation: ColumnRotation) (panel: Dictionary<(int * int), bool>) = 
    let new_column = new Dictionary<int, bool>()
    [0..5]
    |> List.iter(fun i -> new_column.Add( (i + column_rotation.By) % 6, panel.[(i, column_rotation.Column)]))
    [0..5]
    |> List.iter (fun i -> panel.[(i, column_rotation.Column)] <- new_column.[i])

let run_instruction (panel: Dictionary<(int * int), bool>) instruction = 
    match instruction with 
    | Instruction.LightRectangle(rectangle) -> light_rectangle rectangle panel 
    | Instruction.RotateRow(row) -> rotate_row row panel 
    | Instruction.RotateColumn(column) -> rotate_column column panel 

let parse_line (s: string) = 
    let parts = s.Split([| " "; "="; "x" |], StringSplitOptions.RemoveEmptyEntries) 
    match parts.[0] with 
    | "rect" -> Instruction.LightRectangle({ Row = parts.[2] |> Int32.Parse; Column = parts.[1] |> Int32.Parse })
    | "rotate" -> 
        match parts.[1] with 
        | "row" -> Instruction.RotateRow({ Row = parts.[3] |> Int32.Parse; By = parts.[5] |> Int32.Parse })
        | "column" -> Instruction.RotateColumn({ Column = parts.[2] |> Int32.Parse; By = parts.[4] |> Int32.Parse })
        | _ -> failwith "unsupported rotation"
    | _ -> failwith "unsupported command"

let to_pretty_print lighted =
    match lighted with 
    | true -> "#"
    | _ -> " "

let run_day8() =
    let lines = File.ReadAllLines("Day8.txt")
    let panel = new Dictionary<(int * int), bool>()
    [0..5]
    |> List.iter (fun row -> [0..49] 
                                |> List.iter (fun column -> panel.Add((row, column), false)))

    lines 
    |> Array.map parse_line 
    |> Array.iter (run_instruction panel)

    let lighted = seq { for item in panel -> item} 
                    |> Seq.countBy (fun x -> x.Value)

    printfn "Lighted: %A" lighted

    [0..5]
    |> List.map (fun row -> [0..49] 
                            |> List.map (fun column -> to_pretty_print panel.[(row, column)]) 
                            |> List.reduce (+))
    |> List.iter (fun x -> printfn "%s" x)




    





