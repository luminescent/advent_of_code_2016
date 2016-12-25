module Day25


open System
open System.IO

type Register = string 

type Source = 
    | Value of int 
    | Register of Register

type Instruction =
    | Cpy of Source*Source 
    | Jnz of Source*Source 
    | Inc of Register 
    | Dec of Register
    | Out of Register

let replace map key value = 
    map 
    |> Map.remove key 
    |> Map.add key value 

let parseInstruction (s:string) = 
    let parts = s.Split([|' '|], StringSplitOptions.None)
    match parts.[0] with 
    | "cpy" -> 
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
        Cpy(part1, part2)
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
        Inc(parts.[1])
    | "dec" -> 
        Dec(parts.[1])
    | "out" -> 
        Out(parts.[1])
    | _ -> 
        failwithf "Unknown instruction %s" s 

let tryParseAsInc instruction = 
    match instruction with 
    | Inc(x) -> Some x
    | _ -> None     
let tryParseAsDec instruction = 
    match instruction with 
    | Dec(x) -> Some x
    | _ -> None     
let tryParseAsJnzForRegister instruction = 
    match instruction with 
    | Jnz(a, b) -> 
        match a with 
        | Value(_) -> None 
        | Register(r) -> 
            match b with 
            | Register(_) -> None
            | Value(v) -> Some (r, v)
    | _ -> None 

let buildMultiplication currentInstruction (instructions: Instruction list) registerValues = 
    match currentInstruction + 2 >= instructions.Length with 
    | true -> None 
    | false ->
        let incInstruction = tryParseAsInc instructions.[currentInstruction] 
        let decInstruction = tryParseAsDec instructions.[currentInstruction + 1]
        let jnzInstruction = tryParseAsJnzForRegister instructions.[currentInstruction + 2]

        match incInstruction.IsSome && decInstruction.IsSome && jnzInstruction.IsSome with 
        | false -> None 
        | true -> 
            let a = incInstruction.Value 
            let b = decInstruction.Value 
            let (c, d) = jnzInstruction.Value 
            match (b = c) && (d = -2) with 
            | true -> Some(a, b)
            | _ -> None 

type AssembunnyResult = 
    | NoClockSignal 
    | RanOutOfInstructions 
    | ClockSignal 

type IsClockSignal = 
    | MaxItemsChecked 
    | YesSoFar 
    | No

// we need to look at the first 2 items of the list only 
let isInfiniteClockSignal maxItemsToCheck outs = 
    match maxItemsToCheck < (outs |> List.length) with 
    | true -> MaxItemsChecked
    | _ -> 
        match outs with 
        | first::second::tail -> 
            match (first = 0 || first = 1) && (first <> second) with
            | true -> YesSoFar
            | false -> No
        | [first] -> 
            match (first = 0) with 
            | true -> YesSoFar
            | _ -> No
        | [] -> YesSoFar
            
// outs theoretically should hold just the last 2 computed values
let rec runInstrcution currentInstruction (instructions: Instruction list) (registerValues: Map<Register, int>) outs maxItemsToCheck = 
    match currentInstruction >= instructions.Length with 
    | true -> RanOutOfInstructions
    | false -> 
        // check we're on infinite 
        match outs |> isInfiniteClockSignal maxItemsToCheck with 
        | MaxItemsChecked -> ClockSignal
        | No -> NoClockSignal
        | YesSoFar ->  
            match instructions.[currentInstruction] with 
            | Cpy(source, destination) -> 
                match destination with 
                | Value(x) ->
                    runInstrcution (currentInstruction + 1) instructions registerValues outs maxItemsToCheck// we just move to the next
                | Register(register) -> 
                    let newValue = 
                        match source with 
                        | Value(value) -> value
                        | Register(register) -> registerValues.[register] 
                    let newRegisterValues = replace registerValues register newValue
                    runInstrcution (currentInstruction + 1) instructions newRegisterValues outs maxItemsToCheck
            | Jnz(source, value) -> 
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
                    runInstrcution (currentInstruction + 1) instructions registerValues outs maxItemsToCheck
                | _ -> 
                    runInstrcution (currentInstruction + jumps) instructions registerValues outs maxItemsToCheck
            | Inc(register) -> 
                // if on an instruction with inc a, followed by dec b, followed by jnz b -2, we update the registers (a = a + b) and b = 0, and jump to currentInstruction + 3     
                let multiplication = buildMultiplication currentInstruction instructions registerValues 
                match multiplication with 
                | None -> 
                    let newValue = registerValues.[register] + 1
                    let newRegisterValues = replace registerValues register newValue
                    runInstrcution (currentInstruction + 1) instructions newRegisterValues outs maxItemsToCheck
                | Some (a, b) -> 
                    // we update the registers and skip 
                    let newValueA = registerValues.[a] + registerValues.[b]
                    let newRegA = replace registerValues a newValueA
                    let newRegB = replace newRegA b 0 
                    runInstrcution (currentInstruction + 3) instructions newRegB outs maxItemsToCheck
            | Dec(register) -> 
                let newValue = registerValues.[register] - 1
                let newRegisterValues = replace registerValues register newValue 
                runInstrcution (currentInstruction + 1) instructions newRegisterValues outs maxItemsToCheck
            | Out(register) -> 
                let newOuts = registerValues.[register] :: outs
                runInstrcution (currentInstruction + 1) instructions registerValues newOuts maxItemsToCheck

let rec findMinRegisterValue registerStartValue instructions = 
    let runForRegisterValue regV instructions = 
        let registers = replace Map.empty ("a") regV 
        let result = runInstrcution 0 instructions registers List.empty 10 // we generate 10 and hope this will be enough to determine an infinite sequence!
        result 

    let result = runForRegisterValue registerStartValue instructions

    match result with 
    | RanOutOfInstructions -> findMinRegisterValue (registerStartValue + 1) instructions
    | NoClockSignal -> findMinRegisterValue (registerStartValue + 1) instructions
    | ClockSignal -> registerStartValue

let run_day25() = 
    let instructions = 
        File.ReadAllLines("Day25.txt")
        |> Array.map parseInstruction
        |> Array.toList 

    let minRegValue = findMinRegisterValue 0 instructions 
    printfn "Minimum register value: %i" minRegValue

