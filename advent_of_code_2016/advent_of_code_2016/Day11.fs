module Day11

(*
allowed moves:
- 2 generators if there aren't orphaning their chips 
- 1 generator if it isn't orpahning its chip 
- 

each gen can move up or down, alone or with its microchip 


*)

open System.Collections.Generic


type Generator = string 
type Microchip = string 

type Floor = {
    Index: int 
    Generators: Set<Generator>
    Microchips: Set<Microchip>
}

type TestingFacility = {
    Floors: Floor list;
}

type Moves = 
    | Generator of Generator: Generator * CurrentFloor: int * NewFloor: int
    | Generators of Generator1: Generator * Generator2: Generator * CurrentFloor: int * NewFloor: int
    | GeneratorMicrochip of Generator: Generator * Microchip: Microchip * CurrentFloor: int * NewFloor: int 
    | Microchip of Microchip: Microchip * CurrentFloor: int * NewFloor: int
    | Microchips of Microchip1: Microchip * Microchip2: Microchip * CurrentFloor: int * NewFloor: int 

let getFloorHash floor = 
    [| floor.Index.ToString() |] 
    |> Array.append (floor.Generators |> Set.toArray |> Array.sort )
    |> Array.append (floor.Microchips |> Set.toArray |> Array.sort )
    |> Array.rev
    |> String.concat ";"

let getTestingFacilityHash testingFacility currentFloor = 
    [| currentFloor.ToString() |]
    |> Array.append ( testingFacility.Floors |> List.sortBy (fun f -> f.Index) |> List.map getFloorHash |> List.toArray)
    |> Array.rev
    |> String.concat ";"


let isFinalState finalFloor generatorsCount microchipsCount testingFacility floor = 
    match floor = finalFloor with 
    | false -> false 
    | _ ->
        let ff = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = floor)).Value
        ff.Generators.Count = generatorsCount && ff.Microchips.Count = microchipsCount 

let isFinalStateTest testingFacility floor = isFinalState 3 2 2 
let isFinalStatePart1 testingFacility floor = isFinalState 3 5 5 

let GlobalStatesOptimalPath = Dictionary<string, int>()

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
                     |> List.append ([ { toF with Microchips = toF.Microchips |> Set.add g } ])
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


let generateGeneratorMoves testingFacility fromFloor toFloor  = 
    (* we can move a generator only if 
        it goes to a floor where there are no chips and it doesn't orphan its own chip 
        or goes to a floor with its own chip 
    *)
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value
        
    let checkGenerator g =
        match fromF.Microchips.Contains(g) with 
        | true -> false 
        | false -> 
             match (toF.Microchips.IsEmpty || (toF.Microchips.Count = 1 && toF.Microchips.Contains(g))) with 
             | true -> true
             | _ -> false 
        
    fromF.Generators 
    |> Set.filter checkGenerator
    |> Set.map (fun g -> Moves.Generator(g, fromFloor, toFloor))
    |> Set.toList


let generateGeneratorsMoves testingFacility fromFloor toFloor = 
    []: Moves list 
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
    (* we can move a microchip if we don't orphan it and only to a floor where either its generator is already there or there are no generators *)
    let fromF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = fromFloor)).Value
    let toF = (testingFacility.Floors |> List.tryFind(fun f -> f.Index = toFloor)).Value

    let checkMicrochip m = 
        match fromF.Generators.Contains(m) with 
        | true -> false
        | _ -> 
            match (toF.Generators.IsEmpty || (toF.Generators.Count = 1 && toF.Generators.Contains(m))) with
            | true -> true
            | _ -> false 

    fromF.Microchips
    |> Set.filter checkMicrochip
    |> Set.map (fun m -> Moves.Microchip(m, fromFloor, toFloor))
    |> Set.toList
                
    
let generateMicrochipsMoves testingFacility fromFloor toFloor = 
    []: Moves list 


let generateNextMoves testingFacility fromFloor toFloor = 
    generateGeneratorMoves testingFacility fromFloor toFloor
    |> List.append (generateGeneratorsMoves testingFacility fromFloor toFloor) 
    |> List.append (generateGeneratorMicrochipMoves testingFacility fromFloor toFloor)
    |> List.append (generateMicrochipMoves testingFacility fromFloor toFloor)
    |> List.append (generateMicrochipsMoves testingFacility fromFloor toFloor)


// this produces a list of valid testingFacilities; each one will count as a move 
//type nextPossibleStates testingFacility currentFloor currentStates globalOptimalStates = 
    
