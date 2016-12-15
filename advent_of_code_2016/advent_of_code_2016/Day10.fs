module Day10

open System 
open System.Collections.Generic
open System.IO

type BotState = 
    | Empty 
    | OneValue of int 
    | LowHigh of int * int 

type Output = { Index: int; mutable Values: Set<int> } 
and Receiver = 
    | Bot of Bot
    | Output of Output  
and Bot = 
    { 
        Index: int; 
        mutable LowReceiver: Receiver option; 
        mutable HighReceiver: Receiver option;
        mutable State: BotState; 
    } 

type Bots = Dictionary<int, Bot>

type Outputs = Dictionary<int, Output> 

let getOrAddBot (bots: Bots) index = 
    match bots.ContainsKey(index) with 
    | true -> bots.[index]
    | _ ->
        let bot = { Index = index; State = BotState.Empty; LowReceiver = None; HighReceiver = None}
        bots.[index] <- bot 
        bot 

let getOrAddOutput (outputs: Outputs) index = 
    match outputs.ContainsKey(index) with 
    | true -> outputs.[index] 
    | _ -> 
        let output = {Index = index; Values = Set.empty}
        outputs.[index] <- output
        output

let addValueToBot bot value = 
    match bot.State with 
    | Empty -> bot.State <- BotState.OneValue(value)
    | OneValue(x) -> bot.State <- BotState.LowHigh(min x value, max x value)
    | LowHigh(x, y) -> failwith (sprintf "trying to put value %A into already filled bot %A" value bot)

let addValueToOutout output value =
    output.Values <- output.Values.Add(value)

let parse_instruction (bots: Bots) (outputs: Outputs) (s: string)  =
    let parts = s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    match parts.Length with 
    | 6 -> 
        let bot = getOrAddBot bots (parts.[5] |> Int32.Parse)
        addValueToBot bot (parts.[1] |> Int32.Parse)
    | _ -> 
        let bot = getOrAddBot bots (parts.[1] |> Int32.Parse)
        match (parts.[5], parts.[10]) with 
        | ("bot", "bot") -> 
            let low_receiver = getOrAddBot bots (parts.[6] |> Int32.Parse)
            bot.LowReceiver <- Some(Receiver.Bot(low_receiver))
            let high_receiver = getOrAddBot bots (parts.[11] |> Int32.Parse)
            bot.HighReceiver <- Some(Receiver.Bot(high_receiver))
        | ("bot", "output") -> 
            let low_receiver = getOrAddBot bots (parts.[6] |> Int32.Parse)
            bot.LowReceiver <- Some(Receiver.Bot(low_receiver))
            let high_receiver = getOrAddOutput outputs (parts.[11] |> Int32.Parse)
            bot.HighReceiver <- Some(Receiver.Output(high_receiver))
        | ("output", "bot") -> 
            let low_receiver = getOrAddOutput outputs (parts.[6] |> Int32.Parse)
            bot.LowReceiver <- Some(Receiver.Output(low_receiver))
            let high_receiver = getOrAddBot bots (parts.[11] |> Int32.Parse)
            bot.HighReceiver <- Some(Receiver.Bot(high_receiver))
        | ("output", "output") -> 
            let low_receiver = getOrAddOutput outputs (parts.[6] |> Int32.Parse)
            bot.LowReceiver <- Some(Receiver.Output(low_receiver))
            let high_receiver = getOrAddOutput outputs (parts.[11] |> Int32.Parse)
            bot.HighReceiver <- Some(Receiver.Output(high_receiver))
        | _ -> 
            failwith (sprintf "Unknow instruction %s" s)

let addValueToReceiver receiver value = 
    match receiver with 
    | Receiver.Bot(bot) -> 
        addValueToBot bot value 
    | Receiver.Output(receiver) -> 
        addValueToOutout receiver value 


let process_bot (bots:Bots) (outputs: Outputs) bot = 
    match bot.State with 
    | BotState.LowHigh(low, high) -> 
        match (low, high) with 
        | (17, 61) -> printfn "Part 1 response: index: %i" bot.Index
        | _ -> ()

        match bot.LowReceiver with 
        | Some(receiver) -> 
            addValueToReceiver receiver low 
        | None -> 
            failwithf "Trying to process filled bot without instructions: %A" bot
        match bot.HighReceiver with 
        | Some(receiver) -> 
            addValueToReceiver receiver high
        | None -> 
            failwithf "Trying to process filled bot without instructions: %A" bot
        bot.State <- BotState.Empty
    | _ ->
        failwithf "Trying to process unfilled bot %A" bot 


let rec trigger_bots (bots:Bots) (outputs: Outputs) = 
    // find first with 2 values and proces it 
    let full_bot = 
        seq { for vals in bots.Values -> vals }  
        |> Seq.tryFind (fun v -> match v.State with 
                                 | BotState.LowHigh(x, y) -> true
                                 | _ -> false)

    match full_bot with 
    | Some(bot) -> 
        process_bot bots outputs bot
        trigger_bots bots outputs 
    | None -> ()


let run_day10() =
    let lines = File.ReadAllLines("Day10.txt")

    let bots = new Bots()
    let outputs = new Outputs()

    lines
    |> Array.iter (parse_instruction bots outputs)

    trigger_bots bots outputs 

    [0; 1; 2]
    |> List.iter (fun i -> printfn "%A" outputs.[i].Values)