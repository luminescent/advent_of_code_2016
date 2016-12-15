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
        failwithf "called generator move without a different move %A" move


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
        failwithf "called generators move without a different move %A" move 

let applyMove testingFacility move =
    match move with 
    | Moves.Generator(_, _, _) -> applyGeneratorMove testingFacility move 
    | Moves.Generators(_, _, _, _) -> applyGeneratorsMove testingFacility move 
    | Moves.GeneratorMicrochip(_, _, _, _) -> applyGeneratorMicrochipMove testingFacility move 
    | Moves.Microchip(_, _, _) -> applyMicrochipMove testingFacility move 
    | Moves.Microchips(_, _, _, _) -> applyMicrochipsMove testingFacility move 


// this produces a list of valid testingFacilities; each one will count as a move 
//type nextPossibleStates testingFacility currentFloor currentStates globalOptimalStates = 
    
