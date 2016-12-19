module Day19

open System.Collections.Generic
open System.Linq
open System.Diagnostics
open System


// http://www.exploringbinary.com/powers-of-two-in-the-josephus-problem/ 
let part1Formula n = 
    let powerOf2 = Math.Floor(log (n) / log (2.0))
    let m2 = 2.0**powerOf2

    let winner = 2.0 * (n - m2) + 1.0
    winner  
     
// https://www.reddit.com/r/adventofcode/comments/5j4lp1/2016_day_19_solutions/dbdi6bb/
let part2Formula n =
    let powerOf3 = Math.Floor(log n/ log 3.0);
    let m3 = 3.0 ** powerOf3;
    let remainder = n - m3;

    match abs (n - m3) < 0.001 with 
    | true -> n
    | false -> max (n - m3)  (2.0 * n - 3.0 * m3)



let isFinishedTour (elfs: List<int>) = 
    match elfs.FindAll(fun e -> e > 0).Count > 1 with 
    | true -> false 
    | _ -> true 

let rec runTour (elfs: List<int>) = 
    match isFinishedTour elfs with 
    | true -> elfs.FindIndex(fun e -> e > 0)
    | _ -> 
        [0..(elfs.Count - 1)]
        |> List.iter (fun i -> 
                        if (elfs.[i] > 0) then 
                            let mutable leftElf = (i + 1) % elfs.Count
                            while elfs.[leftElf] = 0 && leftElf <> i do  
                                leftElf <- (leftElf + 1) % elfs.Count
                            if elfs.[leftElf] > 0 then 
                                elfs.[i] <- elfs.[i] + elfs.[leftElf]
                                elfs.[leftElf] <- 0 
                    )
        runTour elfs

[<DebuggerDisplay("{OriginalIndex}")>]
type Elf = 
    {
        OriginalIndex: int 
        mutable Presents: int 
    }

let rec runElf (elfs: List<Elf>) index = 
    match elfs.Count = 1 with 
    | true -> elfs.[0].OriginalIndex 
    | _ -> 
        let nextElf = (index + elfs.Count / 2) % elfs.Count 

        elfs.[index].Presents <- elfs.[index].Presents + elfs.[nextElf].Presents
        elfs.RemoveAt(nextElf)

        let nextIndex = 
            match index = elfs.Count with 
            | true -> 0 
            | _ -> index + 1

        runElf elfs nextIndex                  


let run_part1() = 
//    let elfs = new List<int>()
//    [0..3001329]
//    |> List.iter (fun _ -> elfs.Add(1))

    let winningElf = part1Formula 3001330.0 //runTour elfs 
    printfn "Takes all the presents: %A" (winningElf)

let run_part2() = 
//    let elfs = new List<Elf>()
//    [0..3001329]
//    |> List.iter (fun i -> elfs.Add({ OriginalIndex = i + 1; Presents = 1}))

    let winningElf = part2Formula 3001330.0   
    printfn "Takes all the presents: %A" winningElf

    

let run_day19() = 
    
    run_part1()
    run_part2()

