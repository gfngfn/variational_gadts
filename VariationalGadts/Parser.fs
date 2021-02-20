module Parser

open FSharp.Core
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

open Syntax


type ParseError = string


let makeRange ((r1, _) : Ast) ((r2, _) : Ast) : Range =
  match (r1, r2) with
  | (ValidRange(posL, _), ValidRange(_, posR)) -> ValidRange(posL, posR)
  | _                                          -> DummyRange


let withRange (p : Parser<'r, 'u>) : Parser<Range * 'r, 'u> =
  pipe3 getPosition p getPosition (fun posL v posR -> (ValidRange(posL, posR), v))


let lowerParser s =
  let isIdentifierFirstChar c = isLower c
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
  p s


let identifierParser s =
  let pMain =
    lowerParser >>= begin function
    | "val"
    | "fun"
    | "let" | "in" | "rec"
    | "if" | "then" | "else"
    | "true" | "false" ->
        fail "reserved word"

    | x ->
        preturn x
    end
  let p = withRange pMain |>> (fun (r, x) -> Ident(r, x))
  (attempt p) s


let constructorParser s =
  let pMain =
    let isConstructorFirstChar c = isUpper c
    let isConstructorChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isConstructorFirstChar isConstructorChar "constructor"
  let p = withRange pMain |>> (fun (r, s) -> Ctor(r, s))
  p s


let typeVariableParser s =
  let p = pstring "'" >>. lowerParser
  p s


let integerParser s =
  let p = withRange (many1Satisfy isDigit |>> int)
  p s


let rec absLevelParser s =
  (letParser <|> ifParser <|> abstractionParser <|> appLevelParser) s


and letParser s =
  let pValBind = pstring "let" .>> spaces >>. valueBindingParser
  let pInner = pstring "in" .>> spaces >>. absLevelParser
  let p = pipe2 pValBind pInner (fun valbind e -> LocalBinding(valbind, e))
  (withRange p) s


and valueBindingParser s =
  let p1 : Parser<bool * Ident, 'u> =
    let pRec : Parser<bool, 'u> =
       opt (pstring "rec" .>> spaces) |>> function
       | None    -> false
       | Some(_) -> true
    pRec .>>. identifierParser .>> spaces .>> pstring "=" .>> spaces
  let p2f ((isRec, ident) : bool * Ident) : Parser<ValueBinding, 'u> =
    absLevelParser |>> (fun e1 ->
      if isRec then
        Rec(ident, e1)
      else
        NonRec(ident, e1))
  let p = p1 >>= p2f
  p s


and ifParser s =
  let p0 = pstring "if" .>> spaces >>. absLevelParser .>> spaces
  let p1 = pstring "then" .>> spaces >>. absLevelParser .>> spaces
  let p2 = pstring "else" .>> spaces >>. absLevelParser .>> spaces
  let p = withRange (pipe3 p0 p1 p2 (fun e0 e1 e2 -> IfThenElse(e0, e1, e2)))
  p s


and abstractionParser s =
  let p1 : Parser<Ident, 'u> =
    pstring "fun" .>> spaces >>. identifierParser .>> spaces .>> pstring "->" .>> spaces
  let p2f (ident : Ident) : Parser<AstMain, 'u> =
    absLevelParser >>= fun e -> preturn (Lambda(ident, e))
  let p = withRange (p1 >>= p2f)
  p s


and appLevelParser s =
  let p =
    many1 bottomLevelParser >>= function
    | []      -> failwith "bug"
    | e :: es -> preturn (List.fold (fun ef ex -> (makeRange ef ex, Apply(ef, ex))) e es)
  p s


and bottomLevelParser s =
  let pIdent = identifierParser >>= fun ident -> match ident with Ident(r, _) -> preturn (r, Var(ident))
  let pCtorApp = constructorApplicationParser |>> fun (r, (ctor, es)) -> (r, Constructor(ctor, es))
  let pNil = (withRange (pstring "[]")) |>> fun (r, _) -> (r, Constructor(Ctor(r, "[]"), []))
  let pTrue = withRange (pstring "true" |>> fun _ -> BaseConstant(BooleanValue(true)))
  let pFalse = withRange (pstring "false" |>> fun _ -> BaseConstant(BooleanValue(false)))
  let pUnitConst = withRange (pstring "()" |>> fun _ -> BaseConstant(UnitValue))
  let pIntConst = integerParser |>> fun (r, n) -> (r, BaseConstant(IntegerValue(n)))
  let pParen = between (pstring "(" .>> spaces) (pstring ")") absLevelParser
  let p = ((pIdent <|> pNil <|> pTrue <|> pFalse <|> pUnitConst <|> pIntConst <|> pParen) .>> spaces) <|> pCtorApp
  p s


and constructorApplicationParser s =
  let pCtor = constructorParser .>> spaces
  let pArg = between (pstring "(" .>> spaces) (pstring ")" .>> spaces) (many absLevelParser)
  let p : Parser<Range * (Constructor * Ast list), 'u> = withRange (pCtor .>>. pArg)
  p s


let rec typeParser s =
  let p : Parser<ManualType, 'u> = arrowLevelTypeParser <|> appLevelTypeParser <|> bottomLevelTypeParser
  p s


and arrowLevelTypeParser s =
  let pMain : Parser<ManualType * ManualType, 'u> =
    appLevelTypeParser .>> (pstring "->" .>> spaces) .>>. arrowLevelTypeParser
  let p = withRange (pMain |>> fun (mnty1, mnty2) -> MFuncType(mnty1, mnty2))
  p s


and appLevelTypeParser s =
  let p1 = identifierParser |>> function Ident(_, tyident) -> tyident
  let p2 = many1 bottomLevelTypeParser
  let p : Parser<ManualType, 'u> = withRange (pipe2 p1 p2 (fun tyident mntys -> MDataType(tyident, mntys)))
  p s


and bottomLevelTypeParser s =
  let pTyVar = withRange (typeVariableParser |>> fun s -> MTypeVar(s))
  let pTy = withRange (identifierParser |>> function Ident(_, tyident) -> MDataType(tyident, []))
  let pParen = between (pstring "(" .>> spaces) (pstring ")") typeParser
  let p : Parser<ManualType, 'u> = (pTyVar <|> pTy <|> pParen) .>> spaces
  p s


let generalizedConstructorBranchParser s =
  let pBar = pstring "|" .>> spaces
  let pCtor = constructorParser .>> spaces
  let pTyVars : Parser<ManualTypeVar list, 'u> = many (typeVariableParser .>> spaces)
  let pParamTys = between (pstring "(" .>> spaces) (pstring ")" .>> spaces) (many typeParser)
  let p = pipe3 (pBar >>. pCtor) pTyVars pParamTys (fun ctor tyvars mntys -> GeneralizedConstructorBranch(ctor, tyvars, mntys))
  p s


let typeBindingParser s =
  let pIdent = (identifierParser .>> spaces) |>> function Ident(_, tyident) -> tyident
  let pBranches = pstring "=" .>> spaces >>. many generalizedConstructorBranchParser
  let p = pipe3 pIdent integerParser pBranches (fun tyident (_, arity) gctorbrs -> Generalized(tyident, arity, gctorbrs))
  p s


let bindingParser s =
  let pValBind = pstring "val" .>> spaces >>. valueBindingParser |>> (fun valbind -> BindValue(valbind))
  let pTyBind = pstring "type" .>> spaces >>. typeBindingParser |>> (fun tybind -> BindType(tybind))
  pValBind s


let parse (s : string) : Result<Binding list, ParseError> =
  match run (spaces >>. many bindingParser .>> eof) s with
  | Success(binds, _, _)   -> Result.Ok(binds)
  | Failure(msg, _, _) -> Result.Error(msg)
