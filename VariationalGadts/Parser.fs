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


let token (s : string) : Parser<unit, unit> =
  pstring s .>> spaces |>> fun _ -> ()


let enclose (p : Parser<'a, unit>) : Parser<'a, unit> =
  between (token "(") (token ")") p


let lowerToken : Parser<string, unit> =
  let isIdentifierFirstChar c = isLower c
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"


let identifierToken : Parser<Ident, unit> =
  let pMain =
    lowerToken >>= begin function
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
  attempt p


let constructorToken : Parser<Constructor, unit> =
  let pMain =
    let isConstructorFirstChar c = isUpper c
    let isConstructorChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isConstructorFirstChar isConstructorChar "constructor"
  withRange pMain |>> (fun (r, s) -> Ctor(r, s))


let typeVariableToken : Parser<ManualTypeVar, unit> =
  pstring "'" >>. lowerToken


let integerToken : Parser<Range * int, unit> =
  withRange (many1Satisfy isDigit |>> int)


let rec absLevelParser : Parser<Ast, unit> =
  fun s ->
  let p = letParser <|> ifParser <|> abstractionParser <|> appLevelParser
  p s


and letParser : Parser<Ast, unit> =
  fun s ->
  let pValBind = token "let" >>. valueBindingParser
  let pInner = token "in" >>. absLevelParser
  let p = pipe2 pValBind pInner (fun valbind e -> LocalBinding(valbind, e))
  (withRange p) s


and valueBindingParser : Parser<ValueBinding, unit> =
  fun s ->
  let p1 : Parser<bool * Ident, unit> =
    let pRec =
       opt (token "rec") |>> function
       | None    -> false
       | Some(_) -> true
    pRec .>>. identifierToken .>> spaces .>> token "="
  let p2f ((isRec, ident) : bool * Ident) : Parser<ValueBinding, unit> =
    absLevelParser |>> (fun e1 ->
      if isRec then
        Rec(ident, e1)
      else
        NonRec(ident, e1))
  let p = p1 >>= p2f
  p s


and ifParser : Parser<Ast, unit> =
  fun s ->
  let p0 = token "if" >>. absLevelParser
  let p1 = token "then" >>. absLevelParser
  let p2 = token "else" >>. absLevelParser
  let p = withRange (pipe3 p0 p1 p2 (fun e0 e1 e2 -> IfThenElse(e0, e1, e2)))
  p s


and abstractionParser : Parser<Ast, unit> =
  fun s ->
  let p1 : Parser<Ident, unit> =
    token "fun" >>. identifierToken .>> spaces .>> token "->"
  let p2f (ident : Ident) : Parser<AstMain, unit> =
    absLevelParser >>= fun e -> preturn (Lambda(ident, e))
  let p = withRange (p1 >>= p2f)
  p s


and appLevelParser : Parser<Ast, unit> =
  fun s ->
  let p =
    many1 bottomLevelParser >>= function
    | []      -> failwith "bug"
    | e :: es -> preturn (List.fold (fun ef ex -> (makeRange ef ex, Apply(ef, ex))) e es)
  p s


and bottomLevelParser : Parser<Ast, unit> =
  fun s ->
  let pIdent = identifierToken >>= fun ident -> match ident with Ident(r, _) -> preturn (r, Var(ident))
  let pCtorApp = constructorApplicationParser |>> fun (r, (ctor, es)) -> (r, Constructor(ctor, es))
  let pNil = (withRange (pstring "[]")) |>> fun (r, _) -> (r, Constructor(Ctor(r, "[]"), []))
  let pTrue = withRange (pstring "true" |>> fun _ -> BaseConstant(BooleanValue(true)))
  let pFalse = withRange (pstring "false" |>> fun _ -> BaseConstant(BooleanValue(false)))
  let pUnitConst = withRange (pstring "()" |>> fun _ -> BaseConstant(UnitValue))
  let pIntConst = integerToken |>> fun (r, n) -> (r, BaseConstant(IntegerValue(n)))
  let pParen = enclose absLevelParser
  let p = ((pIdent <|> pNil <|> pTrue <|> pFalse <|> pUnitConst <|> pIntConst) .>> spaces) <|> pParen <|> pCtorApp
  p s


and constructorApplicationParser : Parser<Range * (Constructor * Ast list), unit> =
  fun s ->
  let pCtor = constructorToken .>> spaces
  let pArg = enclose (many absLevelParser)
  let p = withRange (pCtor .>>. pArg)
  p s


let typeIdentifierToken : Parser<TypeIdent, unit> =
  identifierToken |>> function Ident(_, tyident) -> tyident


let rec typeParser : Parser<ManualType, unit> =
  fun s ->
  let p = arrowTypeParser <|> appLevelTypeParser
  p s


and arrowTypeParser  : Parser<ManualType, unit> =
  fun s ->
  let pMain =
    appLevelTypeParser .>> (token "->") .>>. arrowLevelTypeParser
  let p = withRange (pMain |>> fun (mnty1, mnty2) -> MFuncType(mnty1, mnty2))
  (attempt p) s


and appLevelTypeParser : Parser<ManualType, unit> =
  fun s ->
  let p = appTypeParser <|> bottomLevelTypeParser
  p s


and appTypeParser : Parser<ManualType, unit> =
  fun s ->
  let p1 = typeIdentifierToken .>> spaces
  let p2 = many1 bottomLevelTypeParser
  let p = withRange (pipe2 p1 p2 (fun tyident mntys -> MDataType(tyident, mntys)))
  (attempt p) s


and bottomLevelTypeParser : Parser<ManualType, unit> =
  fun s ->
  let pTyVar = withRange (typeVariableToken |>> fun s -> MTypeVar(s))
  let pNullary = withRange (identifierToken |>> function Ident(_, tyident) -> MDataType(tyident, []))
  let pParen = enclose typeParser
  let p = ((pTyVar <|> pNullary) .>> spaces) <|> pParen
  p s


let generalizedConstructorBranchParser : Parser<GeneralizedConstructorBranch, unit> =
  let pCtor = token "|" >>. constructorToken .>> spaces
  let pTyVars = many (typeVariableToken .>> spaces)
  let pParamTys = sepBy typeParser (token ",")
  let pRet = token ":" >>. (typeIdentifierToken .>> spaces) .>>. (many typeParser)
  pipe4 pCtor pTyVars pParamTys pRet (fun ctor tyvars mntys (tyident, mntyrets) -> GeneralizedConstructorBranch(ctor, tyvars, mntys, tyident, mntyrets))


let typeBindingParser : Parser<TypeBinding, unit> =
  fun s ->
  let pIdent = typeIdentifierToken .>> spaces
  let pBranches = token "=" >>. many generalizedConstructorBranchParser
  let p = pipe3 pIdent (integerToken .>> spaces) pBranches (fun tyident (_, arity) gctorbrs -> Generalized(tyident, arity, gctorbrs))
  p s


let bindingParser : Parser<Binding, unit> =
  let pValBind = token "val" >>. valueBindingParser |>> (fun valbind -> BindValue(valbind))
  let pTyBind = token "type" >>. typeBindingParser |>> (fun tybind -> BindType(tybind))
  pValBind <|> pTyBind


let parse (s : string) : Result<Binding list, ParseError> =
  match run (spaces >>. many bindingParser .>> eof) s with
  | Success(binds, _, _)   -> Result.Ok(binds)
  | Failure(msg, _, _) -> Result.Error(msg)
