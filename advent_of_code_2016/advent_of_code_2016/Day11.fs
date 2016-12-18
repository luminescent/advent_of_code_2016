module Day11

open System.Collections.Generic

let rec combinations l n = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations xs (k-1) ) @ combinations xs k 

type Generator = string 
type Microchip = string 

type Floor = {
    Index: int 
    Generators: Set<Generator>
    Microchips: Set<Microchip>
}

type TestingFacility = {
    Floors: Floor list;
    StepsCount: int; 
    CurrentFloor: int; 
}

type Moves = 
    | Generator of Generator: Generator * CurrentFloor: int * NewFloor: int
    | Generators of Generator1: Generator * Generator2: Generator * CurrentFloor: int * NewFloor: int
    | GeneratorMicrochip of Generator: Generator * Microchip: Microchip * CurrentFloor: int * NewFloor: int 
    | Microchip of Microchip: Microchip * CurrentFloor: int * NewFloor: int
    | Microchips of Microchip1: Microchip * Microchip2: Microchip * CurrentFloor: int * NewFloor: int 

let getFloorHash floor = 
    [| floor.Index.ToString(); "Floor; " |] 
    |> Array.append (floor.Generators |> Set.toArray |> Array.sort )
    |> Array.append [| " Microchips: " |]
    |> Array.append (floor.Microchips |> Set.toArray |> Array.sort )
    |> Array.append [| "Generators: " |]
    |> Array.rev
    |> String.concat ";"

let getTestingFacilityHash testingFacility = 
    [| testingFacility.CurrentFloor.ToString(); "Current Floor: " |]
    |> Array.append ( testingFacility.Floors |> List.sortBy (fun f -> f.Index) |> List.map getFloorHash |> List.toArray)
    |> Array.rev
    |> String.concat ";"


let isFinalState finalFloor generatorsCount microchipsCount testingFacility = 
    match testingFacility.CurrentFloor = finalFloor with 
    | false -> false 
    | _ ->
        let ff = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = finalFloor)).Value
        ff.Generators.Count = generatorsCount && ff.Microchips.Count = microchipsCount 

let isFinalStateTest testingFacility = isFinalState 3 2 2 testingFacility 
let isFinalStatePart1 testingFacility = isFinalState 3 5 5 testingFacility

type GlobalStatesOptimalPath = Dictionary<string, int>

let applyGeneratorMove testingFacility move = 
    match move with 
    | Moves.Generator(g, fromFloor, toFloor) ->
        let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
        let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        {
            Floors = testingFacility.Floors
                     |> List.filter (fun f -> f.Index <> fromFloor && f.Index <> toFloor)
                     |> List.append ([ { fromF with Generators = fromF.Generators |> Set.remove g  } ])
                     |> List.append ([ { toF with Generators = toF.Generators |> Set.add g } ])
            StepsCount = testingFacility.StepsCount + 1
            CurrentFloor = toFloor
        }
    | _ -> 
        failwithf "called generator move without a different move %A" move

let applyGeneratorsMove testingFacility move =
    match move with 
    | Moves.Generators(g1, g2, fromFloor, toFloor) ->
        let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
        let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        {
            Floors = testingFacility.Floors
                     |> List.filter (fun f -> f.Index <> fromFloor && f.Index <> toFloor)
                     |> List.append ([ { fromF with Generators = fromF.Generators |> Set.remove g1 |> Set.remove g2  } ])
                     |> List.append ([ { toF with Generators = toF.Generators |> Set.add g1 |> Set.add g2 } ])
            StepsCount = testingFacility.StepsCount + 1
            CurrentFloor = toFloor
        }
    | _ -> 
        failwithf "called generators move without a different move %A" move

let applyGeneratorMicrochipMove testingFacility move = 
    match move with 
    | Moves.GeneratorMicrochip(g, m, fromFloor, toFloor) ->
        let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
        let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        {
            Floors = testingFacility.Floors
                     |> List.filter (fun f -> f.Index <> fromFloor && f.Index <> toFloor)
                     |> List.append ([ { fromF with Microchips = fromF.Microchips |> Set.remove m; Generators = fromF.Generators |> Set.remove g  } ])
                     |> List.append ([ { toF with Microchips = toF.Microchips |> Set.add m; Generators = toF.Generators |> Set.add g } ])
            StepsCount = testingFacility.StepsCount + 1
            CurrentFloor = toFloor
        }
    | _ -> 
        failwithf "called generator microchip move without a different move %A" move

let applyMicrochipMove testingFacility move = 
    match move with 
    | Moves.Microchip(m, fromFloor, toFloor) ->
        let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
        let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        {
            Floors = testingFacility.Floors
                     |> List.filter (fun f -> f.Index <> fromFloor && f.Index <> toFloor)
                     |> List.append ([ { fromF with Microchips = fromF.Microchips |> Set.remove m  } ])
                     |> List.append ([ { toF with Microchips = toF.Microchips |> Set.add m } ])
            StepsCount = testingFacility.StepsCount + 1
            CurrentFloor = toFloor
        }
    | _ -> 
        failwithf "called microchip move without a different move %A" move


let applyMicrochipsMove testingFacility move = 
    match move with 
    | Moves.Microchips(m1, m2, fromFloor, toFloor) ->
        let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
        let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        {
            Floors = testingFacility.Floors
                     |> List.filter (fun f -> f.Index <> fromFloor && f.Index <> toFloor)
                     |> List.append ([ { fromF with Microchips = fromF.Microchips |> Set.remove m1 |> Set.remove m2  } ])
                     |> List.append ([ { toF with Microchips = toF.Microchips |> Set.add m1 |> Set.add m2 } ])
            StepsCount = testingFacility.StepsCount + 1
            CurrentFloor = toFloor
        }
    | _ -> 
        failwithf "called microchips move without a different move %A" move 

let applyMove testingFacility move =
    match move with 
    | Moves.Generator(_, _, _) -> applyGeneratorMove testingFacility move 
    | Moves.Generators(_, _, _, _) -> applyGeneratorsMove testingFacility move 
    | Moves.GeneratorMicrochip(_, _, _, _) -> applyGeneratorMicrochipMove testingFacility move 
    | Moves.Microchip(_, _, _) -> applyMicrochipMove testingFacility move 
    | Moves.Microchips(_, _, _, _) -> applyMicrochipsMove testingFacility move 


let floorIsValid floor = 
    // floor is valid if it does not contain an unprotected chip 
    let friedChip = 
        floor.Microchips
        |> Set.filter (fun m -> (floor.Generators.Count > 0) && not(floor.Generators.Contains(m)))
        |> Set.count
    friedChip = 0 


let generateGeneratorMoves testingFacility fromFloor toFloor  = 
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        
    let checkGenerator g = 
        match fromF.Microchips.Contains(g) && fromF.Generators.Count > 1 with 
        | true -> false  // we orphan its chip with other generators around 
        | _ -> // we check the destinatoins to not fry anything 
            match toF.Generators.Count > 0 with
            | true -> true // it's safe to move it there as all other chips, if any, are already protected (since we move to a valid state)
            | _ -> match toF.Microchips.Count with 
                    | 0 -> true
                    | 1 -> toF.Microchips.Contains (g) // its pair
                    | _ -> false    // we fry other chips 

        
    fromF.Generators 
    |> Set.filter checkGenerator
    |> Set.map (fun g -> Moves.Generator(g, fromFloor, toFloor))
    |> Set.toList


let generateGeneratorsMoves testingFacility fromFloor toFloor = 
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value

    let checkGenerators g1 g2 = 
        match (fromF.Microchips.Contains(g1) || fromF.Microchips.Contains(g2)) && fromF.Generators.Count > 2 with 
        | true -> false 
        | false -> 
            match (toF.Generators.Count = 0) && (toF.Microchips.Remove(g1).Remove(g2).Count > 0) with 
            | true -> false 
            | _ -> false  

    let generatorPairs = combinations (fromF.Generators |> Set.toList ) 2 

    generatorPairs
    |> List.filter (fun pair -> checkGenerators pair.[0] pair.[1])
    |> List.map (fun pair -> Moves.Generators(pair.[0], pair.[1], fromFloor, toFloor))
 


let generateGeneratorMicrochipMoves testingFacility fromFloor toFloor = 
    (*
        we can move only matching pairs 
    *)
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
    fromF.Generators
    |> Set.filter (fun g -> fromF.Microchips.Contains(g))
    |> Set.map (fun g -> Moves.GeneratorMicrochip(g, g, fromFloor, toFloor))
    |> Set.toList 
     
let generateMicrochipMoves testingFacility fromFloor toFloor = 
    (* make sure it goes to a floor with 0 generators or its pair generator *)
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value

    let checkMicrochip m =         
        match (toF.Generators.IsEmpty || (toF.Generators.Contains(m))) with
        | true -> true
        | _ -> false 

    fromF.Microchips
    |> Set.filter checkMicrochip
    |> Set.map (fun m -> Moves.Microchip(m, fromFloor, toFloor))
    |> Set.toList
                
    
let generateMicrochipsMoves testingFacility fromFloor toFloor = 
    (* when we move chips away 
        if their generator is on the floor, we have to check we're not frying the remainers  
     *) 
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value

    let checkMicrochips m1 m2 = 
        match (toF.Generators.Count = 0) || (toF.Generators.Contains(m1) && toF.Generators.Contains(m2)) with 
        | true -> true
        | _ -> false 

    let microchipPairs = combinations (fromF.Microchips |> Set.toList ) 2 

    microchipPairs
    |> List.filter (fun pair -> checkMicrochips pair.[0] pair.[1])
    |> List.map (fun pair -> Moves.Microchips(pair.[0], pair.[1], fromFloor, toFloor))


let generateNextMoves testingFacility toFloor = 
    let fromFloor = testingFacility.CurrentFloor
    let moves = 
        generateGeneratorMoves testingFacility fromFloor toFloor
        |> List.append (generateGeneratorsMoves testingFacility fromFloor toFloor) 
        |> List.append (generateGeneratorMicrochipMoves testingFacility fromFloor toFloor)
        |> List.append (generateMicrochipMoves testingFacility fromFloor toFloor)
        |> List.append (generateMicrochipsMoves testingFacility fromFloor toFloor)
    //printfn "Moves: %A" (moves |> List.map (fun m -> sprintf "%A" m) |> String.concat ", ")
    moves 


let getOrAddHashIfSmaller hash steps (globalState: GlobalStatesOptimalPath) = 
    match globalState.ContainsKey hash with 
    | false -> 
        globalState.Add(hash, steps)
    | _ -> 
        let value = globalState.[hash] 
        if (value > steps) then 
            globalState.[hash] <- steps 
    globalState.[hash]


let bfs start finalStateCheck = 
    let mutable minSteps = 40 
    let q = new Queue<TestingFacility>()
    q.Enqueue(start);

    let visited = new Dictionary<string, int>() 

    while q.Count > 0 do 
        let testingFacility = q.Dequeue()
        match finalStateCheck testingFacility with 
        | true -> 
            printfn "Found a solution with %A steps" testingFacility.StepsCount
            if testingFacility.StepsCount < minSteps then 
                minSteps <- testingFacility.StepsCount 
        | _ -> 
            let hash = getTestingFacilityHash testingFacility 
            if not (visited.ContainsKey hash) || visited.[hash] > testingFacility.StepsCount then 
                visited.[hash] <- testingFacility.StepsCount
                let nextFloors = 
                    match testingFacility.CurrentFloor with 
                    | 0 -> [ 1 ]
                    | 3 -> [ 2 ]
                    | _ -> [testingFacility.CurrentFloor + 1; testingFacility.CurrentFloor - 1]
                let moves = 
                    nextFloors
                    |> List.collect (fun floor -> (generateNextMoves testingFacility floor) |> List.map (fun m -> (floor, m)))
                let appliedMoves = 
                    moves 
                    |> List.map (fun (floor, move) -> applyMove testingFacility move)
                    |> List.filter (fun tf -> tf.StepsCount < minSteps) 
                
                appliedMoves
                |> List.iter (fun tf -> q.Enqueue(tf))   
    minSteps


let rec solve finalTestCheck testingFacility currentFloor (currentStates: Set<string>) (globalOptimalStates: GlobalStatesOptimalPath) currentStepsCount = 
    match currentStepsCount > 40 with
    | true -> () // this is a bad one so we drop it 
    | false -> 
        match finalTestCheck testingFacility currentFloor with 
        | true -> // update the global minimum 
            let stateHash = getTestingFacilityHash testingFacility  
            let currentOptimalPathToCurrentState = getOrAddHashIfSmaller stateHash currentStepsCount globalOptimalStates
            printfn "Optimal number of steps to a solution found so far: %A" currentOptimalPathToCurrentState 
        | _ -> 
            let stateHash = getTestingFacilityHash testingFacility  
            // is our performance way crappier than the globalStateOne? 
            let currentOptimalPathToCurrentState = getOrAddHashIfSmaller stateHash currentStepsCount globalOptimalStates

            match currentOptimalPathToCurrentState < currentStepsCount with 
            | true -> () // this will halt processing as this tree is suboptimal 
            | _ -> 
                // are we in a state that we've seen before? => infinite loop 
                match currentStates.Contains stateHash with 
                | true -> () // we halt processing on this tree 
                | false -> 
                    let newCurrentStates = currentStates.Add stateHash 
                    // we generate all possible states 
                    let nextFloors = 
                        match currentFloor with 
                        | 0 -> [ 1 ]
                        | 3 -> [ 2 ]
                        | _ -> [currentFloor + 1; currentFloor - 1]
                
                    //printfn "%A" currentStates
                    let moves = 
                        nextFloors
                        |> List.collect (fun floor -> (generateNextMoves testingFacility floor) |> List.map (fun m -> (floor, m)))
                    let appliedMoves = 
                        moves 
                        |> List.map (fun (floor, move) -> (floor, applyMove testingFacility move))

                    appliedMoves
                        |> List.iter (fun (floor, newState) -> solve finalTestCheck newState floor newCurrentStates globalOptimalStates (currentStepsCount + 1))
                                 
let run_for_test() =      
    let testingFacility = {
        Floors = 
            [
                { Index = 0; Generators = Set.ofList [] ; Microchips = Set.ofList ["H"; "L"] }
                { Index = 1; Generators = Set.ofList ["H"] ; Microchips = Set.ofList [] }
                { Index = 2; Generators = Set.ofList ["L"] ; Microchips = Set.ofList [] }
                { Index = 3; Generators = Set.ofList [] ; Microchips = Set.ofList [] }
            ];
        CurrentFloor = 0;
        StepsCount = 0;            
    }

    printfn "Min no of moves: %A" (bfs testingFacility isFinalStateTest)

let run_part1() = 
    let testingFacility = {
        Floors = 
            [
                { Index = 0; Generators = Set.ofList ["PR"] ; Microchips = Set.ofList ["PR"] }
                { Index = 1; Generators = Set.ofList ["CO"; "CR"; "RU"; "PL"] ; Microchips = Set.ofList [] }
                { Index = 2; Generators = Set.ofList [] ; Microchips = Set.ofList ["CO"; "CR"; "RU"; "PL"] }
                { Index = 3; Generators = Set.ofList [] ; Microchips = Set.ofList [] }
            ];
        CurrentFloor = 0;
        StepsCount = 0;   
    }

    printfn "Min no of moves: %A" (bfs testingFacility isFinalStatePart1)

let run_day11() =

    // run_for_test()
    run_part1()

//

//
//
//
//    run_part1()
