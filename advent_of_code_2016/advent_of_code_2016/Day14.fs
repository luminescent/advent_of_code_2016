module Day14

open System 
open System.Collections.Generic

type Next1000Hashes = Dictionary<int, string>

let computeMd5 (s: string) = 
    use md5 = System.Security.Cryptography.MD5.Create()
    s
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let computeHash salt i = 
    let md5Hash = (sprintf "%s%i" salt i) |> computeMd5
    md5Hash.ToLower()

let computeHashPart2 salt i = 
    [1..2016]
    |> List.fold (fun acc _ -> computeMd5(acc).ToLower()) (computeHash salt i)


let getFirstTriplet (s: string) = 
    s.ToCharArray()
    |> Array.tryFind(fun c -> s.Contains(System.String(c, 3)))
    

let isKey item (next1000: Next1000Hashes) = 
    match item |> getFirstTriplet with 
    | None -> false
    | Some(c) -> 
        let quintet = System.String(c, 5)
        seq { for v in next1000.Values -> v}
        |> Seq.tryFind (fun hash -> hash.Contains(quintet))
        |> Option.isSome

let rec findKey salt computeHash keyRank index (next1000: Next1000Hashes) = 
    match keyRank with 
    | 0 -> index - 1 
    | _ -> 
        if next1000.Count = 0 then
            [index..(index + 1000)]
            |> List.iter (fun i -> next1000.[i] <- (computeHash salt i))

        let currentHash = next1000.[index]

        index |> next1000.Remove |> ignore
        next1000.[index + 1000] <- computeHash salt (index + 1000)

        let newKeyRank = 
            match isKey currentHash next1000 with 
            | true ->
                printfn "found key for index %i" index 
                keyRank - 1
            | _ -> keyRank

        findKey salt computeHash newKeyRank (index + 1) next1000 

let run_day14() = 
    let index = findKey "abc" computeHashPart2 65 0 (new Dictionary<int, string>())
    printfn "%i" index 








