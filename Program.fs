module Main

open System
open FSharp.Core

type ResultBuilder() =
  member this.Bind(f, x) = Result.bind x f
  member this.Return(v) = Result.Ok(v)

let result = new ResultBuilder()

[<EntryPoint>]
let main argv =
  let input = "fun x -> fun y -> z (y x)"
  let res =
    result {
      let! e = Parser.parse input
      printfn "Hello world %O" e
      return ()
    }
  match res with
  | Ok(_) -> 0 // return an integer exit code
  | _     -> 1
