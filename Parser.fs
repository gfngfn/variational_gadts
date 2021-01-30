module Parser

open FParsec.Primitives
open FParsec.CharParsers

type Ast =
  | Var    of string
  | Apply  of Ast * Ast
  | Lambda of string * Ast

let identifierParser s =
  let isIdentifierFirstChar c = isLetter c
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") s

let rec absLevelParser s =
  (abstractionParser <|> appLevelParser) s

and abstractionParser s =
  let p1 = (pstring "fun" .>> spaces) >>. identifierParser .>> spaces .>> (pstring "->" .>> spaces)
  (p1 >>= fun x -> absLevelParser >>= fun e -> preturn (Lambda(x, e))) s

and appLevelParser s =
  let p =
    many1 bottomLevelParser >>= function
    | []      -> failwith "bug"
    | e :: es -> preturn (List.foldBack (fun ef ex -> Apply(ef, ex)) es e)
  in
  p s

and bottomLevelParser s =
  let p1 = identifierParser >>= fun x -> preturn (Var(x))
  let p2 = between (pstring "(" .>> spaces) (pstring ")") absLevelParser
  ((p1 <|> p2) .>> spaces) s

let from s =
  match run absLevelParser s with
  | Success(res, _, _) -> sprintf "success: %A" res
  | Failure(msg, _, _) -> sprintf "failure: %s" msg
