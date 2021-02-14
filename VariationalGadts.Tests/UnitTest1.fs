module VariationalGadts.Tests

open NUnit.Framework

open MyUtil

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
  let input = "let apply = fun x -> fun y -> x 0 (cons y []) in apply"
  let tyenv = Primitives.initialTypeEnvironment
  let res =
    result {
      let! e = Parser.parse input |> Result.mapError (fun _ -> ())
      let! tyGot = TypeChecker.typecheck tyenv e |> Result.mapError (fun _ -> ())
      return tyGot
    }
  match res with
  | Ok(_)    -> Assert.Pass()
  | Error(_) -> Assert.Fail()
