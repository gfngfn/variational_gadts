module Syntax

open System.Collections.Generic
open FParsec


type Range =
  | DummyRange
  | ValidRange of Position * Position

  override this.ToString () =
    match this with
    | DummyRange ->
        "DUMMY"

    | ValidRange(posL, posR) ->
        let ln1 = posL.Line
        let ln2 = posR.Line
        if ln1 = ln2 then
          sprintf "%d:%d-%d" ln1 posL.Column posR.Column
        else
          sprintf "%d:%d-%d:%d" ln1 posL.Column ln2 posR.Column


type Ident =
  | Ident of Range * string

  override this.ToString () =
    match this with
    | Ident(r, x) -> sprintf "Ident(%O, \"%s\")" r x


type BaseConstant =
  | UnitValue
  | IntegerValue of int


type Ast =
  Range * AstMain


and AstMain =
  | Var    of Ident
  | Apply  of Ast * Ast
  | Lambda of Ident * Ast
  | LetIn  of Ident * Ast * Ast
  | LetRecIn of Ident * Ast * Ast
  | BaseConstant of BaseConstant

  override this.ToString () =
    match this with
    | Var(Ident(_, x)) -> sprintf "Var(\"%s\")" x
    | Apply(e1, e2)    -> sprintf "Apply(%O, %O)" e1 e2
    | Lambda(ident, e) -> sprintf "Lambda(%O, %O)" ident e
    | LetIn(i, e1, e2) -> sprintf "LetIn(%O, %O, %O)" i e1 e2
    | LetRecIn(i, e1, e2) -> sprintf "LetRecIn(%O, %O, %O)" i e1 e2
    | BaseConstant(bc)    -> sprintf "BaseConstant(%O)" bc


type FreeId private(n : int) =

  static let mutable current = 0

  new () =
    current <- current + 1
    new FreeId(current)

  member this.Number = n

  override this.ToString () =
    sprintf "'%d" n


type BoundId private(n : int) =
  static let mutable current = 0

  new () =
    current <- current + 1
    new BoundId(current)

  member this.Number = n

  override this.ToString () =
    sprintf "#%d" n


type DataTypeId =
  | UnitTypeId
  | IntTypeId
  | ListTypeId


type Type<'a> =
  Range * TypeMain<'a>


and TypeMain<'a> =
  | TypeVar  of 'a
  | DataType of DataTypeId * Type<'a> list
  | FuncType of Type<'a> * Type<'a>

  override this.ToString () =
    match this with
    | TypeVar(tv)         -> sprintf "TypeVar(%O)" tv
    | DataType(dtid, tys) -> sprintf "BaseType(%O, %O)" dtid tys
    | FuncType(ty1, ty2)  -> sprintf "FuncType(%O, %O)" ty1 ty2


type MonoTypeVarUpdatable =
  | Free of FreeId
  | Link of MonoType

  override this.ToString () =
    match this with
    | Free(fid) -> sprintf "%O" fid
    | Link(ty)  -> sprintf "%O" ty


and MonoTypeVar =
  | Updatable of MonoTypeVarUpdatable ref

  override this.ToString () =
    match this with
    | Updatable(tvuref) ->
        sprintf "%O" !tvuref


and MonoType =
  Type<MonoTypeVar>


type PolyTypeVar =
  | Mono  of MonoTypeVar
  | Bound of BoundId


type PolyType =
  Type<PolyTypeVar>


type TypeEnv =
  Map<string, PolyType>


let unitType rng =
  (rng, DataType(UnitTypeId, []))


let intType rng =
  (rng, DataType(IntTypeId, []))


let instantiate (pty : PolyType) : MonoType =
  let bidDict = new Dictionary<BoundId, MonoTypeVarUpdatable ref>()
  let rec aux pty =
    let (rng, ptyMain) = pty
    match ptyMain with
    | TypeVar(ptv) ->
        match ptv with
        | Mono(tv) ->
            (rng, TypeVar(tv))

        | Bound(bid) ->
            let tvuref =
              if bidDict.ContainsKey(bid) then
                bidDict.Item(bid)
              else
                let tvuref =
                  let fid = new FreeId()
                  ref (Free(fid))
                bidDict.Add(bid, tvuref)
                tvuref
            (rng, TypeVar(Updatable(tvuref)))

    | DataType(dtid, ptys) ->
        (rng, DataType(dtid, ptys |> List.map aux))

    | FuncType(pty1, pty2) ->
        (rng, FuncType(aux pty1, aux pty2))
  in
  aux pty


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
        tyenv |> Map.fold begin fun acc x pty ->
          acc || occursPoly fid pty
        end false
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
