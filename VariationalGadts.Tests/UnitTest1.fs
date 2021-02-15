module VariationalGadts.Tests

open NUnit.Framework

open MyUtil

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
  let check input =
    let tyenv = Primitives.initialTypeEnvironment
    let res =
      result {
        let! e = Parser.parse input |> Result.mapError (fun _ -> ())
        let! tyGot = TypeChecker.typecheck tyenv e |> Result.mapError (fun _ -> ())
        return tyGot
      }
    match res with
    | Ok(_)    -> true
    | Error(_) -> false

  Assert.IsTrue(
    check """
      let apply = fun x -> fun y -> x 0 (cons y []) in apply
    """)
  Assert.IsTrue(
    check """
      let rec foldl = fun f -> fun acc -> fun xs ->
        decompose_list xs
          (fun u -> acc)
          (fun y -> fun ys -> foldl f (f acc y) ys)
      in
      foldl
    """)
