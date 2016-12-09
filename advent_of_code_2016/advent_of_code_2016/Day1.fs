module Day1

type Facing =
    | N 
    | S
    | W
    | E

type Turn = 
    | R
    | L 

let move (x, y, facing) (turn, positions) = 
    // surely we can write this in one statement, using a signs matrix 
    match facing with 
    | Facing.N -> match turn with 
                    | Turn.L -> (x - positions, y, Facing.W)
                    | Turn.R -> (x + positions, y, Facing.E)
    | Facing.S -> match turn with 
                    | Turn.L -> (x + positions, y, Facing.E)
                    | Turn.R -> (x - positions, y, Facing.W)
    | Facing.W -> match turn with 
                    | Turn.L -> (x, y - positions, Facing.S)
                    | Turn.R -> (x, y + positions, Facing.N)
    | Facing.E -> match turn with 
                    | Turn.L -> (x, y + positions, Facing.N)
                    | Turn.R -> (x, y - positions, Facing.S)


let parse_one (s: string) = 
    let turn = match s.[0] with 
                | 'L' -> Turn.L
                | 'R' -> Turn.R
                | _ -> failwith (sprintf "Unable to parse turn for %s" s)    
    let positions = System.Int32.Parse(s.Substring(1))
    (turn, positions)
     
// we start from (0, 0, Facing.N) 
let parse_all (s: string) = 
    let steps = 
        s.Split([|','; ' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parse_one 
    steps 

let run_course s = 
    let steps = parse_all s 
    let final_position = Array.fold move (0, 0, Facing.N) steps 
    final_position

let rec generate_positions start steps traversed =
    match steps with
    | head :: tail -> 
        let next = (move start head)
        generate_positions next tail (next::traversed)
    | [] -> List.rev traversed

let generate_locations (first:int*int*Facing) (second:int*int*Facing) = 
    let (x1, y1, _) = first
    let (x2, y2, _) = second 
    let all_locations = 
        match x1 = x2 with 
        | true ->  seq { for i in (min y1 y2) .. (max y1 y2) -> (x1, i) }
                    |> Set.ofSeq
        | false -> seq { for i in (min x1 x2) .. (max x1 x2) -> (i, y1) }
                    |> Set.ofSeq
    all_locations 
    |> Set.remove (x1, y1)      // this will be added by the next step 


let rec first_duplicate_position start steps (uniques: Set<int * int>) = 
    match steps with 
    | head :: tail -> 
        let next = (move start head)
        let locations = generate_locations start next
        let intersection = locations |> Set.intersect uniques 

        match intersection.Count with 
        | 0 -> 
            let more_uniques = uniques 
                                |> Set.union locations 
            first_duplicate_position next tail more_uniques 
        | _ -> // here we assume the lines intersect rather than overlap! So essentially intersect will only ever have one element
            intersection |> Set.toSeq |> Seq.head 
    | [] -> 
        let (x, y , _)  = start 
        (x, y)

let find_first_duplicate_position s = 
    let steps = 
        (parse_all s)
        |> Array.toList
    first_duplicate_position (0, 0, Facing.N) steps (Set.empty |> Set.add (0, 0))


let run_course_explicitly s =
    let steps = 
        (parse_all s)
        |> Array.toList
    let positions = generate_positions (0, 0, Facing.N) steps []
    positions 
        |> List.iter (fun p -> printfn "%A" p)
    printfn "----"
     

let total_distance_when_facing (x, y, facing) =
    ((abs x) + (abs y))

let total_distance (x, y) = 
    ((abs x) + (abs y))

let print_solution s = 
    let final_position = run_course s
    printfn "Processing %s" s
    printfn "Final position: %A; distance = %d" final_position (total_distance_when_facing final_position)
    printfn "-----"

let print_solution_2 s = 
    printfn "Processing %s" s
    let first_duplicate = find_first_duplicate_position s 
    printfn "First duplicate: %A; distance = %d" first_duplicate (total_distance first_duplicate)
    printfn "-----"


let run_day1() =
    run_course_explicitly "R2, L3"
    run_course_explicitly "R2, R2, R2"
    run_course_explicitly "R5, L5, R5, R3"

    print_solution "R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1, R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3, R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2, R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2, R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4, L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1"
    print_solution "R8, R4, R4, R8"

    run_course_explicitly "R8, R4, R4, R8"
    print_solution_2 "R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1, R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3, R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2, R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2, R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4, L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1"

