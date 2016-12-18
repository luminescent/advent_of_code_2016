module Day18

open System 

type Tile = 
    | Trap = '^'
    | Safe = '.'

let parseInput (s:string) = 
    s.ToCharArray()
    |> Array.map (fun c -> 
                    match c with 
                    | '^' -> Tile.Trap
                    | '.' -> Tile.Safe
                    | _ -> failwithf "Unknown character %A" c)

let deduceTile (t1, t2, t3) = 
    match (t1, t2, t3) with 
    | (Tile.Trap, Tile.Trap, Tile.Safe) -> Tile.Trap
    | (Tile.Safe, Tile.Trap, Tile.Trap) -> Tile.Trap
    | (Tile.Trap, Tile.Safe, Tile.Safe) -> Tile.Trap
    | (Tile.Safe, Tile.Safe, Tile.Trap) -> Tile.Trap
    | _ -> Tile.Safe 

let constructNewRow (row: Tile[]) = 
    let innerValues = 
        seq { for i in 1..(row.Length - 2) -> i}
        |> Seq.map (fun i -> deduceTile (row.[i - 1], row.[i], row.[i + 1]))
        |> Seq.toArray 

    [ 
        [| deduceTile (Tile.Safe, row.[0], row.[1]) |] ; 
        innerValues ; 
        [| deduceTile (row.[row.Length - 2], row.[row.Length - 1], Tile.Safe) |] ]
    |> Array.concat  

let countSafeTilesOnRow row = 
    row 
    |> Array.filter (fun t -> t = Tile.Safe) 
    |> Array.length

let rec countSafeTiles steps row total: int = 
    match steps with
    | 1 -> total + (row |> countSafeTilesOnRow)
    | _ -> 
        let newRow = constructNewRow row 
        let newTotal = total + (row |> countSafeTilesOnRow)
        countSafeTiles (steps - 1) newRow newTotal 


let run_day18() = 
    let input = parseInput "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"

    printfn "%A" ( countSafeTiles 400000  input 0 )



    // printfn "%A" input 