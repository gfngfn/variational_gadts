module VariationalGadts.Tests

open NUnit.Framework

open MyUtil

[<SetUp>]
let Setup () =
    ()

[<TestCase(
  "let apply = fun x -> fun y -> x 0 (cons y []) in apply"
)>]
[<TestCase(
  """
  let rec foldl = fun f -> fun acc -> fun xs ->
    decompose_list xs
      (fun u -> acc)
      (fun y -> fun ys -> foldl f (f acc y) ys)
  in
  foldl
  """
)>]
let Test1 (input : string) =
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
