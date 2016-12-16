module Day16

let rec generateDiskData capacity (seed: int list) = 
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
    let checksum = 
        [0..2..(data.Length - 1)]
        |> List.map (fun i -> (data.[i] + data.[i + 1] + 1) % 2 )
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
//    let gen = generateDiskData 25 [1; 1; 1; 1; 0; 0; 0; 0; 1; 0; 1; 0]
//    printfn "%A" gen 

//    printfn "%A" (generateChecksum [1; 1; 0; 0; 1; 0; 1; 1; 0; 1; 0; 0])
    prettyPrintChecksum (getDiscChecksum 35651584 [1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0])
