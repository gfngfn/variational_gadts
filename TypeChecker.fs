module TypeChecker

open FSharp.Core

open MyUtil
open Syntax


type TypeError =
  | UnboundVariable of Range
  | Inclusion       of FreeId * MonoType * MonoType
  | Contradiction   of MonoType * MonoType


type UnificationError =
  | InternalInclusion     of FreeId
  | InternalContradiction


let freshMonoType (rng : Range) : MonoType =
  let fid = new FreeId()
  let tvuref = ref (Free(fid))
  (rng, TypeVar(Updatable(tvuref)))


let unify (ty1 : MonoType) (ty2 : MonoType) : Result<unit, TypeError> =
  let rec aux (ty1 : MonoType) (ty2 : MonoType) : Result<unit, UnificationError> =
    let (rng1, tyMain1) = ty1
    let (rng2, tyMain2) = ty2
    match (tyMain1, tyMain2) with
    | (TypeVar(Updatable{contents = Link(ty1sub)}), _) ->
        aux ty1sub ty2

    | (_, TypeVar(Updatable{contents = Link(ty2sub)})) ->
        aux ty1 ty2sub

    | (TypeVar(Updatable({contents = Free(fid1)} as tvuref1)), TypeVar(Updatable{contents = Free(fid2)})) ->
        if fid1 = fid2 then
          Ok()
        else
          tvuref1 := Link(ty2)
          Ok()

    | (TypeVar(Updatable({contents = Free(fid1)} as tvuref1)), _) ->
        if occurs fid1 ty2 then
          Error(InternalInclusion(fid1))
        else
          tvuref1 := Link(ty2)
          Ok()

    | (_, TypeVar(Updatable({contents = Free(fid2)} as tvuref2))) ->
        if occurs fid2 ty1 then
          Error(InternalInclusion(fid2))
        else
          tvuref2 := Link(ty1)
          Ok()

    | (BaseType(bty1), BaseType(bty2)) ->
        if bty1 = bty2 then Ok() else Error(InternalContradiction)

    | (FuncType(ty11, ty12), FuncType(ty21, ty22)) ->
        result {
          let! () = aux ty11 ty21
          let! () = aux ty12 ty22
          return ()
        }

    | _ ->
        Error(InternalContradiction)
  in
  aux ty1 ty2 |> Result.mapError begin function
  | InternalInclusion(fid) -> Inclusion(fid, ty1, ty2)
  | InternalContradiction  -> Contradiction(ty1, ty2)
  end


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

  | LetIn(Ident(rngx, x), e1, e2) ->
      result {
        let! ty1 = typecheck tyenv e1
        let pty1 = generalize tyenv ty1
        let! ty2 = typecheck (tyenv.Add(x, pty1)) e2
        return ty2
      }

  | LetRecIn(Ident(rngx, x), e1, e2) ->
      let ty = freshMonoType rngx
      let tyenv = tyenv.Add(x, lift ty)
      result {
        let! ty1 = typecheck tyenv e1
        let! () = unify ty1 ty
        let pty1 = generalize tyenv ty1
        let! ty2 = typecheck tyenv e2
        return ty2
      }
