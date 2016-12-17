module Day16

let rec generateDiskData capacity (seed: int list) = 
    printfn "Capacity for seed length %A" seed.Length
    match capacity <= seed.Length with 
    | true -> seed |> List.take capacity
    | false -> 
        let newData = 
            seed 
            |> List.rev 
            |> List.map (fun x -> (x + 1) % 2) 
            |> List.append [0]
            |> List.append seed 
        generateDiskData capacity newData 

let rec generateChecksum (data: int list) = 
    printfn "checksum for data length %A" data.Length
    let checksum = 
        data
        |> Seq.ofList
        |> Seq.chunkBySize 2
        |> Seq.map(fun x -> (x.[0] + x.[1] + 1) % 2)
        |> Seq.toList
    match (checksum.Length % 2) with 
    | 1 -> checksum
    | _ -> generateChecksum checksum    

let getDiscChecksum capacity (seed: int list) = 
    (generateDiskData capacity seed)
    |> generateChecksum
 
let prettyPrintChecksum (checksum: int list) = 
    checksum
    |> List.iter (fun i -> printf "%i" i)

let run_day16() = 
    prettyPrintChecksum (getDiscChecksum 35651584 [1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0])
    