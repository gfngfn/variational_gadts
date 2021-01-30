module Main

open System

[<EntryPoint>]
let main argv =
    let message = Lib.from "3.14" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
