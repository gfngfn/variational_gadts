module Main

open System
open FSharp.Core

open MyUtil

type ProgramError =
  | ParseError of Parser.ParseError
  | TypeError  of TypeChecker.TypeError

[<EntryPoint>]
let main argv =
  let input = "fun x -> fun y -> z (y x)"
  let tyenv = Map.empty
  let res =
    result {
      let! e = Parser.parse input |> Result.mapError (fun x -> ParseError(x))
      printfn "Hello world %O" e
      let! ty = TypeChecker.typecheck tyenv e |> Result.mapError (fun x -> TypeError(x))
      return ()
    }
  match res with
  | Ok(_) -> 0 // return an integer exit code
  | _     -> 1
