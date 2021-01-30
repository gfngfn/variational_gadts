module TypeChecker

open FSharp.Core

open MyUtil
open Syntax

type TypeEnv =
  Map<string, PolyType>

type TypeError =
  | UnboundVariable of Range


let freshMonoType (rng : Range) : MonoType =
  let fid = fresh ()
  let tvuref = ref (Free(fid))
  (rng, TypeVar(Updatable(tvuref)))


let unify (ty1 : MonoType) (ty2 : MonoType) : Result<unit, TypeError> =
  failwith "TODO: unify"


let rec typecheck (tyenv : TypeEnv) (e : Ast) : Result<MonoType, TypeError> =
  let (rng, eMain) = e in
  match eMain with
  | Var(Ident(_, x)) ->
      match tyenv.TryFind(x) with
      | Some(pty) ->
          let ty = instantiate pty
          Ok(ty)

      | None ->
          Error(UnboundVariable(rng))

  | Lambda(Ident(rngx, x), e0) ->
      let ty = freshMonoType rngx
      result {
        let! ty0 = typecheck (tyenv.Add(x, lift ty)) e0
        return (rng, FuncType(ty, ty0))
      }

  | Apply(e1, e2) ->
      result {
        let! ty1 = typecheck tyenv e1
        let! ty2 = typecheck tyenv e2
        let tyR = freshMonoType rng
        let! () = unify ty1 (DummyRange, FuncType(ty2, tyR))
        return tyR
      }
