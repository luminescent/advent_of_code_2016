module Day12

open System
open System.IO

type Register = string 

type Source = 
    | Value of int 
    | Register of Register

type Instruction =
    | Cpy of Source*Register 
    | Jnz of Source*Source 
    | Inc of Register 
    | Dec of Register 

let replace map key value = 
    map 
    |> Map.remove key 
    |> Map.add key value 

let parseInstruction (s:string) = 
    let parts = s.Split([|' '|], StringSplitOptions.None)
    match parts.[0] with 
    | "cpy" -> 
        let isInt, value = Int32.TryParse(parts.[1]) 
        match isInt with
        | true -> Instruction.Cpy(Source.Value(value), parts.[2])
        | _ -> Instruction.Cpy(Source.Register(parts.[1]), parts.[2])
    | "jnz" -> 
        let isInt, value = Int32.TryParse(parts.[1]) 
        let isIntSource, sourceValue = Int32.TryParse(parts.[2])
        let part1 = 
            match isInt with 
            | true -> Value(value)
            | _ -> Register(parts.[1])
        let part2 = 
            match isIntSource with 
            | true -> Value(sourceValue)
            | _ -> Register(parts.[2])
        Jnz(part1, part2)
    | "inc" -> 
        Instruction.Inc(parts.[1])
    | "dec" -> 
        Instruction.Dec(parts.[1])
    | _ -> 
        failwithf "Unknown instruction %s" s 

let rec runInstrcution currentInstruction (instructions: Instruction list) (registerValues: Map<Register, int>) = 
    match currentInstruction >= instructions.Length with 
    | true -> registerValues 
    | false -> 
        match instructions.[currentInstruction] with 
        | Instruction.Cpy(source, register) -> 
            let newValue = 
                match source with 
                | Value(value) -> value
                | Register(register) -> registerValues.[register] 
            let newRegisterValues = replace registerValues register newValue
            runInstrcution (currentInstruction + 1) instructions newRegisterValues 
        | Instruction.Jnz(source, value) -> 
            let x = 
                match source with 
                | Value(value) -> value
                | Register(register) -> registerValues.[register] 
            let jumps = 
                match value with 
                | Value(x) -> x
                | Register(r) -> registerValues.[r]
            match x with 
            | 0 -> 
                runInstrcution (currentInstruction + 1) instructions registerValues 
            | _ -> 
                runInstrcution (currentInstruction + jumps) instructions registerValues 
        | Instruction.Inc(register) -> 
            let newValue = registerValues.[register] + 1
            let newRegisterValues = replace registerValues register newValue
            runInstrcution (currentInstruction + 1) instructions newRegisterValues
        | Instruction.Dec(register) -> 
            let newValue = registerValues.[register] - 1
            let newRegisterValues = replace registerValues register newValue 
            runInstrcution (currentInstruction + 1) instructions newRegisterValues

let run_day12() = 
    let lines = File.ReadAllLines("Day12.txt")

    let instructions = 
        lines
        |> Array.map parseInstruction
        |> Array.toList

//    let registerValues = runInstrcution 0 instructions (Map.empty |> Map.add "a" 0 |> Map.add "b" 0 |> Map.add "c" 0 |> Map.add "d" 0 )
//    printfn "Register value for a: %i" registerValues.["a"]

    let registerValues2 = runInstrcution 0 instructions (Map.empty |> Map.add "a" 0 |> Map.add "b" 0 |> Map.add "c" 1 |> Map.add "d" 0 )
    printfn "Register value for a: %i" registerValues2.["a"]



        

