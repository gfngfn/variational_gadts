module VariationalGadts.Tests

open NUnit.Framework

open MyUtil
open Syntax


let findValue (x : string) (tyenv : TypeEnv) : Result<PolyType, string> =
  match tyenv.TryFindValue(x) with
  | Some(pty) -> Ok(pty)
  | None      -> Error(sprintf "findValue: %s not found" x)


let freshPolyType () =
  let rng = DummyRange
  let bid = new BoundId()
  (rng, TypeVar(Bound(bid)))


let checkMain (input : string) : Result<TypeEnv, string> =
  let tyenv = Primitives.initialTypeEnvironment
  result {
    let! binds = Parser.parse input |> Result.mapError (sprintf "parse error: %s")
    let! tyenv = TypeChecker.typecheckBindingList tyenv binds |> Result.mapError (sprintf "type error: %O")
    return tyenv
  }


let check (input : string) (x : string) (ptyExpect : PolyType) =
  let res =
    result {
      let! tyenv = checkMain input
      let! ptyGot = findValue x tyenv
      return ptyGot
    }
  match res with
  | Ok(ptyGot) ->
      let sExpect = TypeConv.showPolyType ptyExpect
      let sGot = TypeConv.showPolyType ptyGot
      (TypeConv.equalPoly ptyGot ptyExpect, sprintf "expected: %s, got: %s" sExpect sGot)

  | Error(errmsg) ->
      (false, errmsg)


let parse (input : string) =
  match Parser.parse input with
  | Ok(_)      -> (true, "")
  | Error(msg) -> (false, msg)


let pass (input : string) =
  match checkMain input with
  | Ok(_)      -> (true, "")
  | Error(msg) -> (false, msg)


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
let ``typing a conditional expression`` () =
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


[<Test>]
let ``parsing a type binding 1`` () =
  let input =
    """
    type result :: 1 =
      | Ok    'v 'e ('v) : result 'v 'e
      | Error 'v 'e ('e) : result 'v 'e
    """
  let (b, msg) = pass input
  Assert.IsTrue(b, msg)

[<Test>]
let ``parsing a type binding 2`` () =
  let input =
    """
    type ast :: 1 =
      | App 'a 'b (ast ('a -> 'b), ast 'a) : ast 'b
      | Const 'a  ('a)                     : ast 'a
    """
  let (b, msg) = pass input
  Assert.IsTrue(b, msg)
