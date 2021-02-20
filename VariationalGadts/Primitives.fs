module Primitives

open Syntax


let dummy =
  DummyRange


let freshBoundType () =
  let bid = new BoundId()
  let pty = (dummy, TypeVar(Bound(bid)))
  (bid, pty)


let primitiveValues : (string * PolyType) list =
  [
    begin
      let (_, pty) = freshBoundType ()
      let ptyList = listType dummy pty
      ("cons", pty --> (ptyList --> ptyList))
    end;
    begin
      let (_, pty) = freshBoundType ()
      ("is_empty", listType dummy pty --> boolType dummy)
    end;
    begin
      let (_, ptyElem) = freshBoundType ()
      let (_, ptyRes) = freshBoundType ()
      let ptyList = listType dummy ptyElem
      let ptyNilFun = unitType dummy --> ptyRes
      let ptyConsFun = ptyElem --> (ptyList --> ptyRes)
      ("decompose_list", ptyList --> (ptyNilFun --> (ptyConsFun --> ptyRes)))
    end;
  ]


let primitiveConstructors : (string * ConstructorDef) list =
  [
    begin
      let (bid, pty) = freshBoundType ()
      let ctordef =
        {
          BoundIds = [bid];
          MainType = listType dummy pty;
          ArgTypes = [];
        }
      ("[]", ctordef)
    end;
  ]


let initialTypeEnvironment : TypeEnv =
  let tyenv = TypeEnv.empty
  let tyenv =
    primitiveValues |> List.fold (fun (tyenv : TypeEnv) ((x, pty) : string * PolyType) ->
      tyenv.AddValue(x, pty)
    ) tyenv
  let tyenv =
    primitiveConstructors |> List.fold (fun (tyenv : TypeEnv) (ctor, ctordef) ->
      tyenv.AddConstructor(ctor, ctordef)
    ) tyenv
  tyenv
