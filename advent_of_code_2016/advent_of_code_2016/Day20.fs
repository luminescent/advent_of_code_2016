module Day20

open System.IO
open System

type Segment = Int64*Int64

let intersectsOrTouches s a = 
    let (x, y) = s 
    (x - 1L <= a && a <= y + 1L)

let segmentsIntersectOrTouch s1 s2 = 
    let (x1, y1) = s1 
    let (x2, y2) = s2 

    x1 |> intersectsOrTouches s2 
    || y1 |> intersectsOrTouches s2 
    || x2 |> intersectsOrTouches s1 
    || y2 |> intersectsOrTouches s1 


let mergeSegments (s1: Segment) (s2: Segment) = 
    let (x1, y1) = s1 
    let (x2, y2) = s2 
    (min x1 x2, max y1 y2)    

let processSegment (segments: Segment list) segment = 
    let computeIntersections = 
        segments 
        |> List.map (fun s -> (s, segmentsIntersectOrTouch s segment))

    let intersections =
        computeIntersections
        |> List.filter snd
        |> List.map fst
    
    let merged = 
        intersections 
        |> List.fold mergeSegments segment

    let neutral = 
        computeIntersections
        |> List.filter (snd >> not)
        |> List.map fst

    merged :: neutral 


let findMinIpAddress (segments: Segment list)  = 
    let segment0 = 
        segments 
        |> List.find(fun (x, _) -> x = 0L)

    let processedSegments = 
        segments 
        |> List.fold processSegment [segment0]

    let (a, b) = 
        processedSegments 
        |> List.find (fun (x, _) -> x = 0L)

    b + 1L    

let countAddressesBetweenSegments total pair = 
    match pair with 
    | [| (x1, y1); (x2, y2)|] -> total + x2 - y1 - 1L  
    | _ -> failwithf "Unable to parse pair %A" pair 


let countIpAddresses (segments: Segment list) = 
    let processedSegments = 
        segments 
        |> List.fold processSegment [segments |> List.head ]
    
    let orderedSegments = 
        processedSegments        
        |> List.sortBy fst 

    let count = 
        orderedSegments
        |> Seq.ofList
        |> Seq.windowed 2 
        |> Seq.fold countAddressesBetweenSegments 0L

    count 


let parseInput (lines: string[]) = 
    lines
    |> Array.map (fun l -> l.Split([| '-'|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun parts -> (parts.[0] |> Int64.Parse, parts.[1] |> Int64.Parse))
    |> Array.map (fun (x, y) -> (min x y, max x y) )    

let run_day20() = 
    let lines = File.ReadAllLines("Day20.txt")     

    let minIpAddress = 
        lines 
        |> parseInput
        |> Array.toList 
        |> findMinIpAddress 

    printfn "Day 20, part 1: %A" minIpAddress

    let allowedAddressesCount = 
        lines
        |> parseInput
        |> Array.toList
        |> countIpAddresses

    printfn "Day 20, part 2: %A" allowedAddressesCount