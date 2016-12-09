// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Day1
open Day2
open Microsoft.FSharp.Core.Operators 
open System



[<EntryPoint>]
let main argv = 

    //run_day1()    
    run_day2()
    
    Console.ReadLine() |> ignore

    0 // return an integer exit code
