module Day5

open System.Security.Cryptography

let compute_md5 (s: string) = 
    use md5 = System.Security.Cryptography.MD5.Create()
    s
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash

let starts_with_6_zeroes_in_hexa (hash: byte[]) = 
    hash.[0] = 0uy && hash.[1] = 0uy && hash.[2] >= 0uy && hash.[2] <= 15uy 

let get_6th_char_in_hexa (hash: byte[]) = 
    let s = 
        hash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)
    printfn "Hexa hash: %A" s 
    s.Chars(5)

let find_password (s: string) = 
    let rec generate_password seed index (password: char list) = 
        match password.Length with
        | 8 -> 
            password 
            |> List.rev
        | _ -> 
            let current = sprintf "%s%i" seed index 
            let md5 = compute_md5 current 
            match starts_with_6_zeroes_in_hexa md5 with 
            | false -> 
                generate_password seed (index + 1) password
            | true -> 
                printfn "found for index %i - hash %A" index md5 
                let new_password = (get_6th_char_in_hexa md5) :: password 
                generate_password seed (index + 1) new_password

    generate_password s 0 List.empty


let run_day5() = 
    let password = find_password "wtnhxymk"
    printfn "Password for %A: %A" "wtnhxymk" password