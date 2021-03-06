﻿module Day4

open System
open System.Collections.Generic
open System.Linq
open System.IO

type Room = { Segments: string list; SectorId: int; ClaimedCheckSum: string }

let parse_room (s: string) = 
    let parts = s.Split([|"-"; "["; "]"|], StringSplitOptions.RemoveEmptyEntries)
    let room = { 
        Segments = [0..parts.Length - 3] |> List.map (fun i -> parts.[i])
        SectorId = parts.[parts.Length - 2] |> Int32.Parse 
        ClaimedCheckSum = parts.[parts.Length - 1]
    }
    room 

let compute_checksum room = 
    let all_letters = 
        room.Segments 
        |> List.collect (fun s -> s.ToCharArray() |> Array.toList)
        |> List.toArray 

    let rec count_letters (s: char[]) position (dictionary: Dictionary<char, int>) =
        match position < s.Length with 
        | false -> dictionary 
        | true -> 
            let c = s.[position]
            match dictionary.ContainsKey(c) with 
            | false -> dictionary.Add(c, 1)
            | true -> 
                let count = dictionary.[c]
                dictionary.[c] <- count + 1
            count_letters s (position + 1) dictionary 

    let letters_count = count_letters all_letters 0 (new Dictionary<char, int>())

    let top_5_most_frequent_chars = 
        seq { for item in letters_count -> (item.Key, -1 * item.Value) } 
        |> Seq.sortBy (fun i -> snd i, fst i)
        |> Seq.take(5)
        |> Seq.map(fun i -> fst i)
        |> Seq.toArray

    new string(top_5_most_frequent_chars)


let is_valid_room room = 
    room.ClaimedCheckSum = compute_checksum room 

let cipher_char (c:char) positions = 
    let offset = (int)'a'
    let current = (int) c
    let final = ((current - offset) + positions) % 26 + offset 
    (char) (final)

let cipher_str (s: string) positions =  
    String.Concat(
        s.ToCharArray()
        |> Array.map (fun c -> (char) (cipher_char c positions))
    )

let cipher_segments (s: string list) positions = 
        s
        |> List.map(fun a -> cipher_str a positions)
        |> List.toArray 
        |> String.concat " "


let run_day4() = 
    let lines = File.ReadAllLines("Day4.txt")

    let sectorsSum = 
        lines
        |> Array.map parse_room
        |> Array.filter is_valid_room
        |> Array.sumBy (fun r -> r.SectorId)

    printfn "Sum of valid room sector ids: %A" sectorsSum

    let deciphered_list = 
        lines 
        |> Array.map parse_room 
        |> Array.map (fun r -> (cipher_segments r.Segments r.SectorId, r.SectorId))
        |> Array.filter(fun x -> (fst x).Contains("northpole") )

    for x in deciphered_list do
        printfn "%A" x



    