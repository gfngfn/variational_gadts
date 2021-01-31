module Syntax

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

type Ast = Range * AstMain

and AstMain =
  | Var    of Ident
  | Apply  of Ast * Ast
  | Lambda of Ident * Ast
  override this.ToString () =
    match this with
    | Var(Ident(_, x)) -> sprintf "Var(\"%s\")" x
    | Apply(e1, e2)    -> sprintf "Apply(%O, %O)" e1 e2
    | Lambda(ident, e) -> sprintf "Lambda(%O, %O)" ident e

type FreeId private(n : int) =
  static let mutable current = 0
  new () =
    current <- current + 1
    new FreeId(current)
  member this.Number = n
  override this.ToString () =
    sprintf "%d" n

type BoundId = int // temporary

type BaseType =
  | UnitType

type Type<'a> =
  Range * TypeMain<'a>

and TypeMain<'a> =
  | TypeVar  of 'a
  | BaseType of BaseType
  | FuncType of Type<'a> * Type<'a>
  override this.ToString () =
    match this with
    | TypeVar(tv)        -> sprintf "TypeVar(%O)" tv
    | BaseType(bty)      -> sprintf "BaseType(%O)" bty
    | FuncType(ty1, ty2) -> sprintf "FuncType(%O, %O)" ty1 ty2

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

and MonoType = Type<MonoTypeVar>

type PolyTypeVar =
  | Mono  of MonoTypeVar
  | Bound of BoundId

type PolyType = Type<PolyTypeVar>


let rec instantiate (pty : PolyType) : MonoType =
  let (rng, ptyMain) = pty
  match ptyMain with
  | TypeVar(ptv) ->
      match ptv with
      | Mono(tv) ->
          (rng, TypeVar(tv))

      | Bound(_) ->
          failwith "TODO: instantiate"

  | BaseType(bty) ->
      (rng, BaseType(bty))

  | FuncType(pty1, pty2) ->
      (rng, FuncType(instantiate pty1, instantiate pty2))


let rec lift (ty : MonoType) : PolyType =
  let (rng, tyMain) = ty
  match tyMain with
  | TypeVar(tv) ->
      (rng, TypeVar(Mono(tv)))

  | BaseType(bty) ->
      (rng, BaseType(bty))

  | FuncType(ty1, ty2) ->
      (rng, FuncType(lift ty1, lift ty2))
