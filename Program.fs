module Main

open System
open FSharp.Core

open MyUtil

type ProgramError =
  | ParseError of Parser.ParseError
  | TypeError  of TypeChecker.TypeError

[<EntryPoint>]
let main argv =
  let input = "let apply = fun x -> fun y -> x y in apply"
  let tyenv = Map.empty
  let res =
    result {
      let! e = Parser.parse input |> Result.mapError (fun x -> ParseError(x))
      printfn "Expression: %O" e
      let! ty = TypeChecker.typecheck tyenv e |> Result.mapError (fun x -> TypeError(x))
      printfn "Type: %O" ty
      return ()
    }
  match res with
  | Ok(_) ->
      0

  | Error(e) ->
      printfn "Error: %O" e
      1
