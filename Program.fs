module Main

open System

[<EntryPoint>]
let main argv =
    let message = Lib.from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
