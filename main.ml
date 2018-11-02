let load_file filename : string =
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            lines := input_line chan :: !lines
        done; ""
    with End_of_file ->
        close_in chan;
        !lines |> List.rev |> String.concat "\n"

open Parser

type name = string
type arg = string
type op = string

type ast =
| Op of op * arg list
| Label of name

let ast_to_string = function
| Op (op, args) -> op ^ " (" ^ (String.concat ", " args ) ^ ")" 
| Label name -> name ^ ":"

let is = function
| Ok (i,_) -> "YAY " ^ (ast_to_string i) 
| Error r -> "noooop " ^ r


let comment = pChar '#' >>> pAll2(pZeroWhitespace >>> pString) @=> ignore
let op = (pZeroWhitespace >>> pString <|> pAll2 (pWhitespace >>> pString) <<< pZeroWhitespace) @=> fun (name, args)-> Op (name, args)
let label = (pZeroWhitespace >>> pString <<< pChar ':') @=> fun name -> Label name
let line = ((op ||| label) <<<? comment) 

let lines = refl(fun lines -> 
    (((line <<< pChar '\n') <|> lines) @=> fun (a,b) -> a::b)
    ||| (line @=> fun a -> [a]))



let () =
    let o = load_file "test.asm" in
    
    match o |> Parser.to_txt |> lines.parse with
    | Ok (i,_) ->
        let t = List.map ast_to_string i in
        "Yay " ^ String.concat "\n" t |> print_endline
    | Error r -> "noooop " ^ r |> print_endline
    

