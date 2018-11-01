type txt = {
    str : string;
    pos : int;
}
let to_txt txt =  { str = txt; pos = 0 }

type 'a parser = {
    name : string;
    parse : txt -> ('a * txt, string) result; }

let char_to_string = String.make 1

let pChar (ch:char) : char parser =
    let str_ch = char_to_string ch in
    { name = "parse char '" ^ str_ch ^ "'";
      parse = fun txt -> 
        if String.length txt.str <= txt.pos then Error ("Coldn't match char '" ^ str_ch ^ "' as input string is empty")
        else if txt.str.[txt.pos] = ch then Ok (ch, { txt with pos = txt.pos + 1 })
        else Error ("Coldn't match char '" ^ str_ch ^ "' with '"^ (char_to_string txt.str.[txt.pos]) ^"'")}


let (@=>) parser f = { parser with parse = 
                        fun txt -> 
                        match parser.parse txt with 
                        | Ok (v, txt) -> Ok (f v, txt)
                        | Error e-> Error e }

let (@@) parser name  = { parser with name = name }

let pChoose (parsers:'a parser list) : 'a parser =
    let rec parse txt = function
    | [] -> Error ("None of parsers has succeeded: " ^ (parsers |> List.map (fun x->x.name) |> String.concat ", "))
    | p::ps -> match p.parse txt with Ok _ as o -> o | Error _ -> parse txt ps in
    { name = "any parser";
      parse = fun txt -> parse txt parsers; }

let pAll (parsers:'a parser list) : 'a list parser =
    let rec parse acc txt = function
    | [] -> Ok(List.rev acc, txt)
    | p::ps -> match p.parse txt with Ok (v, txt) -> parse (v::acc) txt ps | Error _ as e -> e in
    { name = "any parser";
      parse = fun txt -> parse [] txt parsers; }

let pAll2 (parser:'a parser) : 'a list parser =
    let rec parse acc txt = 
        match parser.parse txt with
        | Ok (v, txt) -> parse (v::acc) txt
        | Error _ as e -> 
            if acc = [] then e
            else Ok (List.rev acc, txt)
    in
    { name = "any parser";
      parse = fun txt -> parse [] txt; }

let pCharRange startChar endChar =
    let startChar = Char.code startChar in
    let endChar = Char.code endChar in
    List.init (endChar - startChar) (fun ch -> Char.chr (startChar + ch) |> pChar)
    |> pChoose
    |> pAll2

let chars_to_string x = String.concat "" (List.map (String.make 1) x)

let pString = pCharRange 'A' 'z' @=> chars_to_string

let pStr str = (String.to_seq str |> List.of_seq |> List.map pChar |> pAll) @@ ("parse string " ^ str)
let pInt = 
    { name = "int parser";
      parse = fun txt -> 
            match (pCharRange '0' '9').parse txt with
            | Ok (v, txt2) ->
                let v = chars_to_string v in begin
                match v |> int_of_string_opt with 
                | Some x -> Ok (x, txt2)
                | None -> Error ("cannot parse '" ^ v ^ "' to int") end
            | Error e -> Error e }


let (<|>) a b = 
    { name = a.name ^ " AND " ^ b.name;
      parse = fun txt -> 
        match a.parse txt with
        | Ok (v1, txt) -> begin match b.parse txt with 
            | Ok (v2, txt) -> Ok ((v1,v2), txt)
            | Error t -> Error t end
        | Error t -> Error t }

let (>>>) a b = (a <|> b) @=> snd
let (<<<) a b =  (a <|> b) @=> fst

let (|||) a b =  pChoose [a;b]

let pInteger = (pStr "0x" >>> pInt) ||| pInt

let pWhitespace = ['\r'; '\n'; ' '; '\t'] |> List.map pChar |> pChoose