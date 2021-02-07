module Primitives

open Syntax


let dummy =
  DummyRange


let (-->) ty1 ty2 =
  (dummy, FuncType(ty1, ty2))


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
    end
  ]


let primitiveConstructors : (Constructor * ConstructorDef) list =
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
    end
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
