module Main

open System
open FSharp.Core

open MyUtil
open Syntax


type ProgramError =
  | ParseError of Parser.ParseError
  | TypeError  of TypeChecker.TypeError


[<EntryPoint>]
let main argv =
  let input = """
let f = fun b -> fun x -> fun y ->
  if b then x else y
in
f
"""
  let tyenv = Primitives.initialTypeEnvironment
  let res =
    result {
      let! binds = Parser.parse input |> Result.mapError (fun x -> ParseError(x))
      printfn "Bindings: %O" binds
      let! tyenv = TypeChecker.typecheckBindingList tyenv binds |> Result.mapError (fun x -> TypeError(x))
      tyenv.FoldValue(
        begin fun () x pty ->
          printfn "val %s : %s" x (TypeConv.showPolyType pty)
        end,
        ())
      return ()
    }
  match res with
  | Ok(_) ->
      0

  | Error(e) ->
      printfn "Error: %O" e
      1
