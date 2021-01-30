module Parser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

type Range = (Position * Position) option

type Ident =
  | Ident of Range * string

type Ast = Range * AstMain

and AstMain =
  | Var    of Ident
  | Apply  of Ast * Ast
  | Lambda of Ident * Ast

let makeRange ((r1, _) : Ast) ((r2, _) : Ast) : Range =
  match (r1, r2) with
  | (Some(posL, _), Some(_, posR)) -> Some(posL, posR)
  | _                              -> None

let identifierParser s =
  let pMain =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
  let p = pipe3 getPosition pMain getPosition (fun posL x posR -> Ident(Some(posL, posR), x))
  p s

let rec absLevelParser s =
  (abstractionParser <|> appLevelParser) s

and abstractionParser s =
  let p1 : Parser<Position * Ident, 'u> =
    let pFun = getPosition .>> (pstring "fun") .>> spaces
    let pIdentAndArrow : Parser<Ident, 'u> = identifierParser .>> spaces .>> pstring "->" .>> spaces
    pFun .>>. pIdentAndArrow
  let p2f (posL, ident) : Parser<Ast, 'u> =
    absLevelParser .>>. getPosition >>= fun (e, posR) -> preturn (Some(posL, posR), Lambda(ident, e))
  (p1 >>= p2f) s

and appLevelParser s =
  let p =
    many1 bottomLevelParser >>= function
    | []      -> failwith "bug"
    | e :: es -> preturn (List.foldBack (fun ef ex -> (makeRange ef ex, Apply(ef, ex))) es e)
  in
  p s

and bottomLevelParser s =
  let p1 = identifierParser >>= fun ident -> match ident with Ident(r, _) -> preturn (r, Var(ident))
  let p2 = between (pstring "(" .>> spaces) (pstring ")") absLevelParser
  ((p1 <|> p2) .>> spaces) s

let from s =
  match run absLevelParser s with
  | Success(res, _, _) -> sprintf "success: %A" res
  | Failure(msg, _, _) -> sprintf "failure: %s" msg
