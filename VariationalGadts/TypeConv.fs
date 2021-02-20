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


let equalMono (ty1 : MonoType) (ty2 : MonoType) : bool =
  let fidDict = new Dictionary<FreeId, FreeId>()
  let rec aux ty1 ty2 =
    let (_, ty1main) = ty1
    let (_, ty2main) = ty2
    match (ty1main, ty2main) with
    | (TypeVar(Updatable{contents = Link(ty1sub)}), _) ->
        aux ty1sub ty2

    | (_, TypeVar(Updatable{contents = Link(ty2sub)})) ->
        aux ty1 ty2sub

    | (TypeVar(Updatable{contents = Free(fid1)}), TypeVar(Updatable{contents = Free(fid2)})) ->
        if fidDict.ContainsKey(fid1) then
          fidDict.Item(fid1) = fid2
        else
          fidDict.Add(fid1, fid2)
          true

    | (DataType(dtid1, tys1), DataType(dtid2, tys2)) ->
        if dtid1 = dtid2 then
          auxList tys1 tys2
        else
          false

    | (FuncType(ty11, ty12), FuncType(ty21, ty22)) ->
        aux ty11 ty21 && aux ty12 ty22

    | _ ->
        false

  and auxList tys1 tys2 =
    try
      List.fold2 (fun b ty1 ty2 ->
        if b then
          aux ty1 ty2
        else
          false
      ) true tys1 tys2
    with
    | _ -> false
  in
  aux ty1 ty2


let equalPoly (pty1 : PolyType) (pty2 : PolyType) : bool =
  let bidDict = new Dictionary<BoundId, BoundId>()
  let rec aux (_, pty1main) (_, pty2main) =
    match (pty1main, pty2main) with
    | (TypeVar(Mono(_)), _) | (_, TypeVar(Mono(_))) ->
        false

    | (TypeVar(Bound(bid1)), TypeVar(Bound(bid2))) ->
        if bidDict.ContainsKey(bid1) then
          bidDict.Item(bid1) = bid2
        else
          bidDict.Add(bid1, bid2)
          true

    | (DataType(dtid1, ptys1), DataType(dtid2, ptys2)) ->
        if dtid1 = dtid2 then
          auxList ptys1 ptys2
        else
          false

    | (FuncType(pty11, pty12), FuncType(pty21, pty22)) ->
        aux pty11 pty21 && aux pty12 pty22

    | _ ->
        false

  and auxList ptys1 ptys2 =
    try
      List.fold2 (fun b pty1 pty2 ->
        if b then
          aux pty1 pty2
        else
          false
      ) true ptys1 ptys2
    with
    | _ -> false
  in
  aux pty1 pty2


type ParenRequirement =
  | Standalone
  | Codomain


let showBaseType = function
  | UnitTypeId -> "unit"
  | BoolTypeId -> "bool"
  | IntTypeId  -> "int"
  | ListTypeId -> "list"


let rec showMonoTypeAux (parenReq : ParenRequirement) (ty : MonoType) =
  let aux = showMonoTypeAux
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


let showMonoType =
  showMonoTypeAux Codomain


let rec showPolyTypeAux (parenReq : ParenRequirement) (pty : PolyType) =
  let aux = showPolyTypeAux
  let (_, ptyMain) = pty
  match ptyMain with
  | TypeVar(Mono(tvuref)) ->
      showMonoTypeAux parenReq (DummyRange, TypeVar(tvuref))

  | TypeVar(Bound(bid)) ->
      bid.ToString()

  | DataType(dtid, tys) ->
      let s = showBaseType dtid in
      match tys with
      | [] ->
          s

      | _ :: _ ->
          let sargs = tys |> List.map (aux Standalone) |> String.concat " "
          sprintf "%s %s" s sargs

  | FuncType(pty1, pty2) ->
       let s1 = aux Standalone pty1
       let s2 = aux Codomain pty2
       match parenReq with
       | Standalone -> sprintf "(%s -> %s)" s1 s2
       | Codomain   -> sprintf "%s -> %s" s1 s2


let showPolyType =
  showPolyTypeAux Codomain
