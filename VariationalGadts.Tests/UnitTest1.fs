module VariationalGadts.Tests

open NUnit.Framework

open MyUtil
open Syntax


let findValue (x : string) (tyenv : TypeEnv) : Result<PolyType, unit> =
  match tyenv.TryFindValue(x) with
  | Some(pty) -> Ok(pty)
  | None      -> Error(())


let freshPolyType () =
  let rng = DummyRange
  let bid = new BoundId()
  (rng, TypeVar(Bound(bid)))


let check (input : string) (x : string) (ptyExpect : PolyType) =
  let tyenv = Primitives.initialTypeEnvironment
  let res =
    result {
      let! binds = Parser.parse input |> Result.mapError (fun _ -> ())
      let! tyenv = TypeChecker.typecheckBindingList tyenv binds |> Result.mapError (fun _ -> ())
      let! ptyGot = findValue x tyenv
      return ptyGot
    }
  match res with
  | Ok(ptyGot) ->
      let sExpect = TypeConv.showPolyType ptyExpect
      let sGot = TypeConv.showPolyType ptyGot
      (TypeConv.equalPoly ptyGot ptyExpect, sprintf "expected: %s, got: %s" sExpect sGot)
  | Error(err) -> (false, sprintf "error: %O" err)


[<SetUp>]
let Setup () =
    ()


[<Test>]
let ``typing apply`` () =
  let input =
    """
    val apply = fun x -> fun y -> x y
    """
  let ident = "apply"
  let ptyExpect =
    let ptyA = freshPolyType ()
    let ptyB = freshPolyType ()
    (ptyA --> ptyB) --> (ptyA --> ptyB)
  let (b, msg) = check input ident ptyExpect
  Assert.IsTrue(b, msg)


[<Test>]
let ``typing foldl`` () =
  let input =
    """
    val rec foldl = fun f -> fun acc -> fun xs ->
      decompose_list xs
        (fun u -> acc)
        (fun y -> fun ys -> foldl f (f acc y) ys)
    """
  let ident = "foldl"
  let ptyExpect =
    let ptyA = freshPolyType ()
    let ptyB = freshPolyType ()
    (ptyA --> (ptyB --> ptyA)) --> (ptyA --> (listType DummyRange ptyB --> ptyA))
  let (b, msg) = check input ident ptyExpect
  Assert.IsTrue(b, msg)


[<Test>]
let ``typing conditionals`` () =
  let input =
    """
    val f = fun b -> fun x -> fun y ->
      if b then x else y
    """
  let ident = "f"
  let ptyExpect =
    let ptyA = freshPolyType ()
    (boolType DummyRange --> (ptyA --> (ptyA --> ptyA)))
  let (b, msg) = check input ident ptyExpect
  Assert.IsTrue(b, msg)
