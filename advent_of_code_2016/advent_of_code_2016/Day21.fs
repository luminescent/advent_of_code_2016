module Day21

open System
open System.IO

type Operation = 
    | SwapPosition of X: int * Y: int
    | SwapLetter of A:char * B:char 
    | RotateLeft of Steps:int
    | RotateRight of Steps:int 
    | RotateBasedOnLetterIndex of Letter:char 
    | ReversePositions of X:int * Y:int
    | MovePosition of X: int * Y:int

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)


let parse (s: string) = 
    let parts = s.Split([| ' '|], StringSplitOptions.RemoveEmptyEntries)

    match parts with 
    | [| "swap"; "position"; x; "with"; "position"; y |] -> Operation.SwapPosition( x |> Int32.Parse, y |> Int32.Parse)
    | [| "swap"; "letter"; a; "with"; "letter"; b |] -> Operation.SwapLetter(a.[0], b.[0])
    | [| "rotate"; "right"; x ; "steps"|] -> Operation.RotateRight(x |> Int32.Parse)
    | [| "rotate"; "right"; x ; "step"|] -> Operation.RotateRight(x |> Int32.Parse)
    | [| "rotate"; "left"; x ; "steps"|] -> Operation.RotateLeft(x |> Int32.Parse)
    | [| "rotate"; "left"; x ; "step"|] -> Operation.RotateLeft(x |> Int32.Parse)
    | [| "rotate"; "based"; "on"; "position"; "of"; "letter"; x |] -> Operation.RotateBasedOnLetterIndex(x.[0])
    | [| "reverse"; "positions"; x; "through"; y |] -> Operation.ReversePositions( x |> Int32.Parse, y |> Int32.Parse)
    | [| "move"; "position"; x; "to"; "position"; y |] -> Operation.MovePosition( x |> Int32.Parse, y |> Int32.Parse)
    | _ -> failwithf "unable to parse %s" s


let rec processOperation password operation = 
    //printfn "currently processing %A with %A" (password |> Array.map (fun c -> c.ToString()) |> String.concat "")  operation 
    match operation with 
    | SwapPosition(x, y) -> 
        password
        |> Array.mapi (fun i c -> 
                        match i with 
                        | a when a = x -> password.[y]
                        | a when a = y -> password.[x]
                        | _ -> c)
    | SwapLetter(a, b) -> 
        password
        |> Array.map (fun c -> 
                        match c with 
                        | x when x = a -> b
                        | x when x = b -> a
                        | _ -> c)
    | RotateRight(steps) -> 
        RotateLeft(password.Length - steps)
        |> processOperation password         
    | RotateLeft(steps) -> 
        password
        |> Array.mapi (fun i c -> password.[(i + steps) % password.Length])          
    | RotateBasedOnLetterIndex(a) -> 
        let position = password |> Array.findIndex (fun c -> c = a)
        let times = 
            match position with 
            | x when x >= 4 -> position + 2
            | _ -> position + 1
        RotateRight(times % password.Length) 
        |> processOperation password
    | ReversePositions(x, y) -> 
        password
        |> Array.mapi (fun i c -> 
                        match i with 
                        | z when z >= x && z <= y -> password.[x + y - z]
                        | _ -> c)
    | MovePosition(x, y) -> 
        match x < y with 
        | true ->
            let p1 = password |> Array.take x 
            let p2 = password |> Array.skip (x + 1) |> Array.take (y - x)
            let p3 = [| password.[x] |]
            let p4 = password |> Array.skip (y + 1) 
            let a = [| p2; p3; p4 |] |> Array.fold Array.append p1 
            a
        | _ -> 
            let p1 = password |> Array.take y 
            let p2 = [| password.[x]|]
            let p3 = password |> Array.skip y |> Array.take (x - y)
            let p4 = password |> Array.skip (x + 1)
            let a = [| p2; p3; p4 |] |> Array.fold Array.append p1 
            a

let processOperations password operations = 
    operations
    |> Array.fold processOperation password

let unscramblePassword password operations = 
    let permutations = 
        password
        |> Array.toList 
        |> permute 
        |> List.map List.toArray

    let unscrambled = 
        permutations
        |> List.find (fun pass -> (processOperations pass operations) = password )
        
    unscrambled 

let run_day21() = 
    let operations = 
        File.ReadAllLines("Day21.txt")
        |> Array.map parse 

    let scrambledPassword =  
        operations
        |> processOperations ("abcdefgh".ToCharArray())

    printfn "Scrambled password: %A" (scrambledPassword |> Array.map (fun c -> c.ToString()) |> String.concat "")

    let unscrambledPassword = unscramblePassword ("fbgdceah".ToCharArray()) operations

    printfn "Unscrambled password: %A" (unscrambledPassword |> Array.map (fun c -> c.ToString()) |> String.concat "")
