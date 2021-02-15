module VariationalGadts.Tests

open NUnit.Framework

open MyUtil
open Syntax

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
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

  let input1 =
    """
    let apply = fun x -> fun y -> x y in apply
    """
  let tyExpect1 =
    let tyA = TypeChecker.freshMonoType DummyRange
    let tyB = TypeChecker.freshMonoType DummyRange
    (tyA --> tyB) --> (tyA --> tyB)
  Assert.IsTrue(check input1 tyExpect1)

  let input2 =
    """
    let rec foldl = fun f -> fun acc -> fun xs ->
      decompose_list xs
        (fun u -> acc)
        (fun y -> fun ys -> foldl f (f acc y) ys)
    in
    foldl
    """
  let tyExpect2 =
    let tyA = TypeChecker.freshMonoType DummyRange
    let tyB = TypeChecker.freshMonoType DummyRange
    (tyA --> (tyB --> tyA)) --> (tyA --> (listType DummyRange tyB --> tyA))
  Assert.IsTrue(check input2 tyExpect2)
