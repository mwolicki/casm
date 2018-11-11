open Parser

type name = string
type arg = Ident of string | Numb of int64

type op = string

type tokens =
| Op of op * arg list
| Label of name
| Directive of name * arg list

let ast_to_string = function
| Op (op, args) -> 
    let args = args |> List.map (function Numb i -> Int64.to_string i | Ident i -> i) in
    op ^ " (" ^ (String.concat ", " args ) ^ ")" 
| Directive (op, args) -> 
    let args = args |> List.map (function Numb i -> Int64.to_string i | Ident i -> i) in
    "!" ^ op ^ " (" ^ (String.concat ", " args ) ^ ")" 
| Label name -> name ^ ":"

let is = function
| Ok (i,_) -> "YAY " ^ (ast_to_string i) 
| Error r -> "noooop " ^ r

let comment = pChar '#' >>> pAll2(pNotChar '\n') @=> ignore
let pArg = pWhitespace >>> ((pIdent @=> fun a -> Ident a) ||| (pInt @=> fun a -> Numb a) ||| (pStringLiteral '"'  @=> fun a -> Ident a))
let directive = 
    (pZeroWhitespace >>> pChar '!' >>> pString <|> pSep pArg ',' <<< pZeroWhitespace) @=> fun (name, args)-> Directive (name, args)
let op = 
    ((pZeroWhitespace >>> pIdent <|> pSep pArg ',' <<< pZeroWhitespace) @=> fun (name, args)-> Op (name, args))
    ||| (pZeroWhitespace >>> pIdent) @=> fun name -> Op(name, [])
let label = (pZeroWhitespace >>> pChar ':' >>> pIdent) @=> fun name -> Label name
let newLineAndEmptyLines = pAll2(pZeroWhitespace <<<? comment <<< pChar '\n')
let line = directive ||| op ||| label
let lines = 
    refl(fun lines -> (((line <<< newLineAndEmptyLines) <|> lines) @=> fun (a,b) -> a::b) ||| (line @=> fun a -> [a]))

let pFile = lines <<< pZeroAll2(pChoose [pIgnore pWhitespace; pIgnore comment;  pIgnore (pChar '\n')])
