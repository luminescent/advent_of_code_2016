module Day23


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
    | Tgl of Register

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
    | "tgl" -> 
        Tgl(parts.[1])
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


let rec runInstrcution currentInstruction (instructions: Instruction list) (registerValues: Map<Register, int>) = 
    match currentInstruction >= instructions.Length with 
    | true -> registerValues 
    | false -> 
        match instructions.[currentInstruction] with 
        | Cpy(source, destination) -> 
            match destination with 
            | Value(x) ->
                runInstrcution (currentInstruction + 1) instructions registerValues // we just move to the next
            | Register(register) -> 
                let newValue = 
                    match source with 
                    | Value(value) -> value
                    | Register(register) -> registerValues.[register] 
                let newRegisterValues = replace registerValues register newValue
                runInstrcution (currentInstruction + 1) instructions newRegisterValues 
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
                runInstrcution (currentInstruction + 1) instructions registerValues 
            | _ -> 
                runInstrcution (currentInstruction + jumps) instructions registerValues 
        | Inc(register) -> 
            // if on an instruction with inc a, followed by dec b, followed by jnz b -2, we update the registers (a = a + b) and b = 0, and jump to currentInstruction + 3     
            let multiplication = buildMultiplication currentInstruction instructions registerValues 
            match multiplication with 
            | None -> 
                let newValue = registerValues.[register] + 1
                let newRegisterValues = replace registerValues register newValue
                runInstrcution (currentInstruction + 1) instructions newRegisterValues
            | Some (a, b) -> 
                // we update the registers and skip 
                let newValueA = registerValues.[a] + registerValues.[b]
                let newRegA = replace registerValues a newValueA
                let newRegB = replace newRegA b 0 
                runInstrcution (currentInstruction + 3) instructions newRegB
        | Dec(register) -> 
            let newValue = registerValues.[register] - 1
            let newRegisterValues = replace registerValues register newValue 
            runInstrcution (currentInstruction + 1) instructions newRegisterValues
        | Tgl(register) -> 
            let instructionToChangeIndex = currentInstruction + registerValues.[register] 
            match instructionToChangeIndex >= instructions.Length with 
            | true -> 
                runInstrcution (currentInstruction + 1) instructions registerValues
            | false ->
                // we now change it 
                let toggledInstruction = 
                    match instructions.[instructionToChangeIndex] with 
                    | Inc(x) -> Dec(x)
                    | Dec(x) -> Inc(x)
                    | Tgl(x) -> Inc(x)
                    | Cpy(s, r) -> Jnz(s, r)
                    | Jnz(s, i) -> Cpy(s, i)
                // build the new instruction list 
                let p1 = instructions |> List.take instructionToChangeIndex 
                let p2 = [toggledInstruction]
                let p3 = instructions |> List.skip (instructionToChangeIndex + 1)
                let newInstructions = [p2; p3] |> List.fold List.append p1 
                runInstrcution (currentInstruction + 1) newInstructions registerValues
            
let run_day23() = 
    let instructions = 
        File.ReadAllLines("Day23.txt")
        |> Array.map parseInstruction
        |> Array.toList 

    let registers = replace  Map.empty ("a") 12 

    let regValues = runInstrcution 0 instructions registers

    printfn "Value for safe: %i" regValues.["a"]
