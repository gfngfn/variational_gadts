module Main

open System

[<EntryPoint>]
let main argv =
  let message = Parser.from "fun x -> fun y -> z (y x)" // Call the function
  printfn "Hello world %s" message
  0 // return an integer exit code
