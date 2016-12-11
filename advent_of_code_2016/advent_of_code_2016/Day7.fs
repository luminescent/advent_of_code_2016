module Day7

open System
open System.IO

type IpAddress = { Sequences: string list; HypernetSequences: string list}

let parse_IpAddress (s: string) = 
    let parts = s.Split([| '['; ']'|], StringSplitOptions.RemoveEmptyEntries)
    {
        Sequences = [0..2..(parts.Length - 1)]
                    |> List.map (fun i -> parts.[i])
        HypernetSequences = [1..2..(parts.Length - 1)]
                            |> List.map (fun i -> parts.[i])
    }

let sequence_contains_abba (s: string) = 
    let is_abba (s: string) =
        s.Chars(0) = s.Chars(3) && s.Chars(1) = s.Chars(2) && s.Chars(0) <> s.Chars(1)

    [0..(s.Length - 4)]
    |> List.tryFind (fun i -> is_abba (s.Substring(i, 4)))
    |> Option.isSome


let supports_tls ipaddress = 
    (ipaddress.Sequences |> List.tryFind sequence_contains_abba).IsSome 
    && (ipaddress.HypernetSequences |> List.tryFind sequence_contains_abba).IsNone 


let sequence_has_aba_and_corresponding_bab (sequence: string) (hypernet_sequences: string list) = 
    let is_aba(s: string) = 
        s.Chars(0) = s.Chars(1) && s.Chars(0) <> s.Chars(1)
    let get_bab(aba: string) = 
        [ aba.Chars(1).ToString(); aba.Chars(0).ToString(); aba.Chars(1).ToString()]
        |> List.reduce (+)
    let contains_fragment(fragment: string)(items: string list) = 
        items
        |> List.tryFind(fun item -> item.Contains(fragment))
        |> Option.isSome

    [0..(sequence.Length - 3)]
    |> List.map (fun i -> sequence.Substring(i, 3))
    |> List.filter is_aba 
    |> List.tryFind (fun aba -> hypernet_sequences |> contains_fragment (get_bab aba) )
    |> Option.isSome

let supports_ssl ipAddress = 
    ipAddress.Sequences
    |> List.tryFind (fun s -> sequence_has_aba_and_corresponding_bab s ipAddress.HypernetSequences)
    |> Option.isSome


let run_day7() = 
    let lines = File.ReadAllLines("Day7.txt")

    let count_supports_tls =
        lines
        |> Array.map parse_IpAddress
        |> Array.filter supports_tls
        |> Array.length 

    printfn "Addresses supporting TLS: %A" count_supports_tls

    let count_supports_ssl = 
        lines
        |> Array.map parse_IpAddress
        |> Array.filter supports_ssl
        |> Array.length 

    printfn "Addresses supporting TLS: %A" count_supports_tls


