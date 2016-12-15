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
    | Microchips of Microchip1: Microchip * Microchip2: Microchip * CurrentFloor: int * NewFloor: int 

let getFloorHash floor = 
    [| floor.Index.ToString() |] 
    |> Array.append (floor.Generators |> Set.toArray |> Array.sort )
    |> Array.append (floor.Microchips |> Set.toArray |> Array.sort )
    |> Array.rev
    |> String.concat ";"

let getTestingFacilityHash testingFacility currentFloor = 
    [| currentFloor.ToString() |]
    |> Array.append ( testingFacility.Floors |> List.map getFloorHash |> List.toArray)
    |> Array.rev
    |> String.concat ";"


let isFinalState finalFloor generatorsCount microchipsCount testingFacility floor = 
    floor = finalFloor 
    && testingFacility.Floors.[floor].Generators.Count = generatorsCount 
    && testingFacility.Floors.[floor].Microchips.Count = microchipsCount 

let isFinalStateTest testingFacility floor = isFinalState 3 2 2 
let isFinalStatePart1 testingFacility floor = isFinalState 3 5 5 

let GlobalStatesOptimalPath = Dictionary<string, int>()

let applyMove testingFacility fromFloor toFloor move =
    

// this produces a list of valid testingFacilities; each one will count as a move 
//type nextPossibleStates testingFacility currentFloor currentStates globalOptimalStates = 
    
