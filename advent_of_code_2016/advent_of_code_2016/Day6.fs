module Day6

open System
open System.IO

let parse_columns (lines: string[]) = 
    let column_count = lines.[0].Length
    [0..(column_count - 1)]
    |> List.map (fun i -> lines 
                            |> Array.map(fun l -> l.[i]))
    |> List.toArray

let get_most_frequent_char (chars: char[]) = 
    chars
    |> Array.countBy id
    |> Array.maxBy snd
    |> fst 

let get_least_frequent_char (chars: char[]) = 
    chars
    |> Array.countBy id
    |> Array.minBy snd
    |> fst 

let run_day6() = 
    let lines = File.ReadAllLines("Day6.txt")

    let message_with_repetition = 
        lines
        |> parse_columns
        |> Array.map get_most_frequent_char
        |> Array.map (fun c -> c.ToString())
        |> Array.reduce (+)

    printfn "Corrected message with repetition %A" message_with_repetition 

    let message_without_repetition = 
        lines
        |> parse_columns
        |> Array.map get_least_frequent_char
        |> Array.map (fun c -> c.ToString())
        |> Array.reduce (+)

    printfn "Corrected message with repetition %A" message_without_repetition

