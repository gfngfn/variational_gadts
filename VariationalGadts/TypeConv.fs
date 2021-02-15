module TypeConv

open Syntax
open System.Collections.Generic

let instantiateScheme (internf : BoundId -> MonoTypeVarUpdatable ref) (pty : PolyType) : MonoType =
  let rec aux pty =
    let (rng, ptyMain) = pty
    match ptyMain with
    | TypeVar(ptv) ->
        match ptv with
        | Mono(tv) ->
            (rng, TypeVar(tv))

        | Bound(bid) ->
            let tvuref = internf bid
            (rng, TypeVar(Updatable(tvuref)))

    | DataType(dtid, ptys) ->
        (rng, DataType(dtid, ptys |> List.map aux))

    | FuncType(pty1, pty2) ->
        (rng, FuncType(aux pty1, aux pty2))
  in
  aux pty


let instantiate (pty : PolyType) : MonoType =
  let bidDict = new Dictionary<BoundId, MonoTypeVarUpdatable ref>()
  let internf (bid : BoundId) =
    if bidDict.ContainsKey(bid) then
      bidDict.Item(bid)
    else
      let tvuref =
        let fid = new FreeId()
        ref (Free(fid))
      bidDict.Add(bid, tvuref)
      tvuref
  instantiateScheme internf pty


let instantiateByMap (bidMap : Map<BoundId, MonoTypeVarUpdatable ref>) : PolyType -> MonoType =
  let internf (bid : BoundId) =
    match bidMap.TryFind(bid) with
    | None          -> failwith "TODO: instantiateByMap, assertion failure"
    | Some(mtvuref) -> mtvuref
  instantiateScheme internf


let rec occurs (fid0 : FreeId) ((_, tyMain) : MonoType) : bool =
  let aux = occurs fid0
  match tyMain with
  | TypeVar(Updatable(tvuref)) ->
      match !tvuref with
      | Free(fid) -> fid = fid0
      | Link(ty)  -> aux ty

  | DataType(_, tys) ->
      tys |> List.tryFind aux |> function
      | None    -> false
      | Some(_) -> true

  | FuncType(ty1, ty2) ->
      let b1 = aux ty1
      let b2 = aux ty2
      b1 || b2


let rec occursPoly (fid0 : FreeId) (pty : PolyType) =
  let aux = occursPoly fid0
  let (_, ptyMain) = pty
  match ptyMain with
  | TypeVar(Mono(Updatable(tvuref))) ->
      match !tvuref with
      | Free(fid) -> fid = fid0
      | Link(ty)  -> occurs fid0 ty

  | TypeVar(Bound(_)) ->
      false

  | DataType(_, ptys) ->
      ptys |> List.tryFind aux |> function
      | None    -> false
      | Some(_) -> true

  | FuncType(pty1, pty2) ->
      aux pty1 || aux pty2


let rec generalizeScheme (genf : FreeId -> BoundId option) (ty : MonoType) : PolyType =
  let aux = generalizeScheme genf
  let (rng, tyMain) = ty
  match tyMain with
  | TypeVar(Updatable(tvuref) as tv) ->
      match !tvuref with
      | Link(tysub) ->
          aux tysub

      | Free(fid) ->
          match genf fid with
          | None      -> (rng, TypeVar(Mono(tv)))
          | Some(bid) -> (rng, TypeVar(Bound(bid)))

  | DataType(dtid, tys) ->
      (rng, DataType(dtid, tys |> List.map aux))

  | FuncType(ty1, ty2) ->
      (rng, FuncType(aux ty1, aux ty2))


let lift =
  generalizeScheme (fun _ -> None)


let generalize (tyenv : TypeEnv) (ty : MonoType) : PolyType =
  let fidDict = new Dictionary<FreeId, BoundId>()
  let genf fid =
    if fidDict.ContainsKey(fid) then
      Some(fidDict.Item(fid))
    else
      let b =
        tyenv.FoldValue(begin fun acc x pty ->
          acc || occursPoly fid pty
        end, false)
      if b then
        None
      else
        let bid = new BoundId()
        fidDict.Add(fid, bid)
        Some(bid)
  generalizeScheme genf ty


type ParenRequirement =
  | Standalone
  | Codomain


let showBaseType = function
  | UnitTypeId -> "unit"
  | BoolTypeId -> "bool"
  | IntTypeId  -> "int"
  | ListTypeId -> "list"


let showMonoType (ty : MonoType) =
  let rec aux (parenReq : ParenRequirement) (ty : MonoType) =
    let (_, tyMain) = ty
    match tyMain with
    | TypeVar(Updatable(tvuref)) ->
        match !tvuref with
        | Free(fid)   -> fid.ToString()
        | Link(tysub) -> aux parenReq tysub

    | DataType(dtid, tys) ->
        let s = showBaseType dtid in
        match tys with
        | [] ->
            s

        | _ :: _ ->
            let sargs = tys |> List.map (aux Standalone) |> String.concat " "
            sprintf "%s %s" s sargs

    | FuncType(ty1, ty2) ->
        let s1 = aux Standalone ty1
        let s2 = aux Codomain ty2
        match parenReq with
        | Standalone -> sprintf "(%s -> %s)" s1 s2
        | Codomain   -> sprintf "%s -> %s" s1 s2
  in
  aux Codomain ty
