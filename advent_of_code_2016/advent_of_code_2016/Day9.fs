module Day9

open System 
open System.IO

type Marker = { Length: int; Times: int}

let get_marker_values (input:string) openingBracket closingBraket = 
    let marker_parts = 
        input.Substring(openingBracket + 1, closingBraket - openingBracket - 1).Split([| 'x' |], StringSplitOptions.RemoveEmptyEntries)
    { Length = marker_parts.[0] |> Int32.Parse; Times = marker_parts.[1] |> Int32.Parse }
        

let rec get_decompress_length (input: string) currentPosition currentLength = 
    match currentPosition = input.Length with 
    | true -> currentLength
    | _ -> 
        match input.Chars(currentPosition) with 
        | '(' -> 
            let closingBracket = input.IndexOf(')', currentPosition)
            let marker = get_marker_values input currentPosition closingBracket
            get_decompress_length input (closingBracket + marker.Length + 1) (currentLength + marker.Length * marker.Times)
        | ' ' ->
            get_decompress_length input (currentPosition + 1) currentLength // not sure where space appears!
        | _ -> 
            get_decompress_length input (currentPosition + 1) (currentLength + 1)


let rec get_decompress_length2 (input: string) currentPosition (currentLength:float) = 
    match currentPosition = input.Length with 
    | true -> currentLength
    | _ -> 
        match input.Chars(currentPosition) with 
        | '(' -> 
            let closingBracket = input.IndexOf(')', currentPosition)
            let marker = get_marker_values input currentPosition closingBracket
            let repeated_pattern = input.Substring(closingBracket + 1, marker.Length)
            let repeated_pattern_decompressed_length = get_decompress_length2 repeated_pattern 0 0.0
            get_decompress_length2 input (closingBracket + marker.Length + 1) (currentLength + repeated_pattern_decompressed_length * (float)marker.Times)
        | ' ' ->
            get_decompress_length2 input (currentPosition + 1) currentLength // not sure where space appears!
        | _ -> 
            get_decompress_length2 input (currentPosition + 1) (currentLength + 1.0)

let run_day9() =
    let input = File.ReadAllText("Day9.txt")

    let length = get_decompress_length input 0 0 
    printfn "Total lenght: %A" length 

    let inner_markers_decompressing_length = get_decompress_length2 input 0 0.0 
    printfn "Total length: %A" inner_markers_decompressing_length