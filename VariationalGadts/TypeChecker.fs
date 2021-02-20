module TypeChecker

open FSharp.Core

open MyUtil
open Syntax


type TypeError =
  | UnboundVariable     of Range * string
  | UnboundConstructor  of Range * string
  | UnboundTypeVariable of Range * ManualTypeVar
  | UnboundTypeIdent    of Range * TypeIdent
  | InvalidArityOfType  of Range * TypeIdent * int * int
  | Inclusion           of FreeId * MonoType * MonoType
  | Contradiction       of MonoType * MonoType
  | ArityMismatch       of Range * int * int


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
        if TypeConv.occurs fid1 ty2 then
          Error(InternalInclusion(fid1))
        else
          tvuref1 := Link(ty2)
          Ok()

    | (_, TypeVar(Updatable({contents = Free(fid2)} as tvuref2))) ->
        if TypeConv.occurs fid2 ty1 then
          Error(InternalInclusion(fid2))
        else
          tvuref2 := Link(ty1)
          Ok()

    | (DataType(dtid1, tys1), DataType(dtid2, tys2)) ->
        if dtid1 = dtid2 then
          auxList tys1 tys2
        else
          Error(InternalContradiction)

    | (FuncType(ty11, ty12), FuncType(ty21, ty22)) ->
        result {
          let! () = aux ty11 ty21
          let! () = aux ty12 ty22
          return ()
        }

    | _ ->
        Error(InternalContradiction)

  and auxList (tys1 : MonoType list) (tys2 : MonoType list) : Result<unit, UnificationError> =
    try
      List.fold2 (fun res ty1 ty2 ->
        result {
          let! () = res
          let! () = aux ty1 ty2
          return ()
        }
      ) (Ok()) tys1 tys2
    with
    | _ -> Error(InternalContradiction)
  in
  aux ty1 ty2 |> Result.mapError begin function
  | InternalInclusion(fid) -> Inclusion(fid, ty1, ty2)
  | InternalContradiction  -> Contradiction(ty1, ty2)
  end


let unifyList (rng : Range) (tys1 : MonoType list) (tys2 : MonoType list) : Result<unit, TypeError> =
  let len1 = List.length tys1
  let len2 = List.length tys2
  if len1 = len2 then
    List.fold2 (fun res ty1 ty2 ->
      result {
        let! () = res
        let! () = unify ty1 ty2
        return ()
      }
    ) (Ok()) tys1 tys2
  else
    Error(ArityMismatch(rng, len1, len2))


let typecheckBaseConstant (rng : Range) (bc : BaseConstant) =
  match bc with
  | UnitValue       -> unitType rng
  | BooleanValue(_) -> boolType rng
  | IntegerValue(_) -> intType rng


let typecheckConstructor (tyenv : TypeEnv) (Ctor(rng, ctor) : Constructor) : Result<MonoType list * MonoType, TypeError> =
  match tyenv.TryFindConstructor(ctor) with
  | None ->
      Error(UnboundConstructor(rng, ctor))

  | Some(ctordef) ->
      let bidMap : Map<BoundId, MonoTypeVarUpdatable ref> =
        ctordef.BoundIds |> List.fold (fun bidMap bid ->
          let tvuref =
            let fid = new FreeId()
            ref (Free(fid))
          bidMap.Add(bid, tvuref)
        ) Map.empty
      let tyArgs = ctordef.ArgTypes |> List.map (TypeConv.instantiateByMap bidMap)
      let tyRet = ctordef.MainType |> TypeConv.instantiateByMap bidMap
      Ok(tyArgs, tyRet)


let rec decodeManualType (tyenv : TypeEnv) (mnty : ManualType) : Result<PolyType, TypeError> =
  let aux = decodeManualType tyenv
  let (rng, mntyMain) = mnty
  match mntyMain with
  | MTypeVar(tyvar) ->
      match tyenv.TryFindTypeVariable(tyvar) with
      | None ->
          Error(UnboundTypeVariable(rng, tyvar))

      | Some(bid) ->
          Ok((rng, TypeVar(Bound(bid))))

  | MDataType(tyident, mntys) ->
      match tyenv.TryFindType(tyident) with
      | None ->
          Error(UnboundTypeIdent(rng, tyident))

      | Some((dtid, arityExpect)) ->
          let arityGot = List.length mntys
          if arityExpect = arityGot then
            result {
              let! ptys = mntys |> List.mapM aux
              return (rng, DataType(dtid, ptys))
            }
          else
            Error(InvalidArityOfType(rng, tyident, arityExpect, arityGot))

  | MFuncType(mnty1, mnty2) ->
      result {
        let! pty1 = aux mnty1
        let! pty2 = aux mnty2
        return (rng, FuncType(pty1, pty2))
      }


let rec typecheck (tyenv : TypeEnv) (e : Ast) : Result<MonoType, TypeError> =
  let (rng, eMain) = e in
  match eMain with
  | BaseConstant(bc) ->
      let ty = typecheckBaseConstant rng bc
      Ok(ty)

  | Constructor(ctor, es) ->
      result {
        let! (tysExpected, tyRes) = typecheckConstructor tyenv ctor
        let! tysGiven = es |> List.mapM (typecheck tyenv)
        let! () = unifyList rng tysGiven tysExpected
        return tyRes
      }

  | Var(Ident(_, x)) ->
      match tyenv.TryFindValue(x) with
      | Some(pty) ->
          let ty = TypeConv.instantiate pty
          Ok(ty)

      | None ->
          Error(UnboundVariable(rng, x))

  | Lambda(Ident(rngx, x), e0) ->
      let ty = freshMonoType rngx
      result {
        let! ty0 = typecheck (tyenv.AddValue(x, TypeConv.lift ty)) e0
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

  | LocalBinding(valbind, e2) ->
      result {
        let! tyenvSub = typecheckValueBinding tyenv valbind
        let! ty2 = typecheck tyenvSub e2
        return ty2
      }

  | IfThenElse(e0, e1, e2) ->
      result {
        let! ty0 = typecheck tyenv e0
        let! () = unify ty0 (boolType DummyRange)
        let! ty1 = typecheck tyenv e1
        let! ty2 = typecheck tyenv e2
        let! () = unify ty1 ty2
        return ty1
      }


and typecheckValueBinding (tyenv : TypeEnv) (valbind : ValueBinding) : Result<TypeEnv, TypeError> =
  match valbind with
  | NonRec(Ident(rngx, x), e1) ->
      result {
        let! ty1 = typecheck tyenv e1
        let pty1 = TypeConv.generalize tyenv ty1
        return (tyenv.AddValue(x, pty1))
      }

  | Rec(Ident(rngx, x), e1) ->
      let ty = freshMonoType rngx
      let tyenvSub = tyenv.AddValue(x, TypeConv.lift ty)
      result {
        let! ty1 = typecheck tyenvSub e1
        let! () = unify ty1 ty
        let pty1 = TypeConv.generalize tyenv ty1
        return tyenv.AddValue(x, pty1)
      }


let typecheckBinding (tyenv : TypeEnv) (bind : Binding) : Result<TypeEnv, TypeError> =
  match bind with
  | BindValue(valbind) ->
      typecheckValueBinding tyenv valbind

  | BindType(tybind) ->
      let (tyident, tyvars, ctorbrs) = tybind
      let dtid = new DataTypeId(tyident)
      let tyenv = tyenv.AddType(tyident, dtid, List.length tyvars)
      let (tyenv, bidacc) : TypeEnv * Alist<BoundId> =
        tyvars |> List.fold (fun (tyenv, bidacc) tyvar ->
          let bid = new BoundId()
          (tyenv.AddTypeVariable(tyvar, bid), bidacc.Extend(bid))
        ) (tyenv, new Alist<BoundId>())
      let bids = bidacc.ToList()
      let typarams = bids |> List.map (fun bid -> (DummyRange, TypeVar(Bound(bid))))
      ctorbrs |> List.fold (fun res ctorbr ->
        match ctorbr with
        | ConstructorBranch(Ctor(_, ctor), mtys) ->
            result {
              let! tyenv = res
              let! ptyacc =
                mtys |> List.fold (fun (res : Result<Alist<PolyType>, TypeError>) mty ->
                  result {
                    let! ptyacc = res
                    let! pty = decodeManualType tyenv mty
                    return ptyacc.Extend(pty)
                  }
                ) (Ok(new Alist<PolyType>()))
              let ctordef =
                {
                  BoundIds = bids;
                  MainType = (DummyRange, DataType(dtid, typarams));
                  ArgTypes = ptyacc.ToList();
                }
              return tyenv.AddConstructor(ctor, ctordef)
            }
      ) (Ok(tyenv))


let typecheckBindingList (tyenv : TypeEnv) (binds : Binding list) : Result<TypeEnv, TypeError> =
  List.fold (fun res bind ->
    result {
      let! tyenv = res
      let! tyenv = typecheckBinding tyenv bind
      return tyenv
    }
  ) (Ok(tyenv)) binds
