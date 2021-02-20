module VariationalGadts.Tests

open NUnit.Framework

open MyUtil
open Syntax


let check input tyExpect =
  let tyenv = Primitives.initialTypeEnvironment
  let res =
    result {
      let! e = Parser.parse input |> Result.mapError (fun _ -> ())
      let! tyGot = TypeChecker.typecheck tyenv e |> Result.mapError (fun _ -> ())
      return tyGot
    }
  match res with
  | Ok(tyGot) -> TypeConv.equalMono tyGot tyExpect
  | Error(_)  -> false


[<SetUp>]
let Setup () =
    ()


[<Test>]
let ``typing apply`` () =
  let input =
    """
    let apply = fun x -> fun y -> x y in apply
    """
  let tyExpect =
    let tyA = TypeChecker.freshMonoType DummyRange
    let tyB = TypeChecker.freshMonoType DummyRange
    (tyA --> tyB) --> (tyA --> tyB)
  Assert.IsTrue(check input tyExpect)


[<Test>]
let ``typing foldl`` () =
  let input =
    """
    let rec foldl = fun f -> fun acc -> fun xs ->
      decompose_list xs
        (fun u -> acc)
        (fun y -> fun ys -> foldl f (f acc y) ys)
    in
    foldl
    """
  let tyExpect =
    let tyA = TypeChecker.freshMonoType DummyRange
    let tyB = TypeChecker.freshMonoType DummyRange
    (tyA --> (tyB --> tyA)) --> (tyA --> (listType DummyRange tyB --> tyA))
  Assert.IsTrue(check input tyExpect)


[<Test>]
let ``typing conditionals`` () =
  let input =
    """
    let f = fun b -> fun x -> fun y ->
      if b then x else y
    in
    f
    """
  let tyExpect =
    let tyA = TypeChecker.freshMonoType DummyRange
    (boolType DummyRange --> (tyA --> (tyA --> tyA)))
  Assert.IsTrue(check input tyExpect)
