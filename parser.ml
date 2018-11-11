type txt = {
    str : string;
    pos : int;
}


let pos_to_string (txt:txt) =
    let rec loop n ln pos txt = 
    if n = 0 then ln, pos 
    else match txt with
        | [] -> ln, pos
        | '\n'::ls -> loop (n - 1) (ln + 1) 1 ls
        | _::ls -> loop (n - 1) ln (pos + 1) ls in
    let (ln, pos) = loop txt.pos 1 1 (txt.str |> String.to_seq |> List.of_seq) in
    " line: " ^ (string_of_int ln) ^ " column: " ^ (string_of_int pos)


let to_txt txt =  { str = txt; pos = 0 }

type 'a parser = {
    name : string;
    parse : txt -> ('a * txt, string) result; }

let char_to_string = String.make 1

let (<|>) a b = 
    { name = a.name ^ " AND " ^ b.name;
      parse = fun txt -> 
        match a.parse txt with
        | Ok (v1, txt) -> begin match b.parse txt with 
            | Ok (v2, txt) -> Ok ((v1,v2), txt)
            | Error t -> Error t end
        | Error t -> Error t }

let (@=>) parser f = { parser with parse = 
                        fun txt -> 
                        match parser.parse txt with 
                        | Ok (v, txt) -> Ok (f v, txt)
                        | Error e-> Error e }

let (@@) parser name  = { name = name;
                          parse = fun txt -> match parser.parse txt with Ok v -> Ok v | Error _ -> Error ("Failed to parse" ^ name ^ " at " ^ (pos_to_string txt)) }

let (>>>) a b = (a <|> b) @=> snd
let (<<<) a b =  (a <|> b) @=> fst

let (<<<?) a b = a <<< {
    name = "try parse " ^ b.name;
    parse = fun txt -> match b.parse txt with Ok (_, txt) -> Ok ((), txt) | Error _ -> Ok ((), txt) }

let pChar (ch:char) : char parser =
    let str_ch = char_to_string ch in
    { name = "parse char '" ^ str_ch ^ "'";
      parse = fun txt -> 
        if String.length txt.str <= txt.pos then Error ("Coldn't match char '" ^ str_ch ^ "' as input string is empty")
        else if txt.str.[txt.pos] = ch then Ok (ch, { txt with pos = txt.pos + 1 })
        else Error ("Coldn't match char '" ^ str_ch ^ "' with '"^ (char_to_string txt.str.[txt.pos]) ^"'")}

let pNotChar (ch:char) : char parser =
    let str_ch = char_to_string ch in
    { name = "parse anything but char '" ^ str_ch ^ "'";
      parse = fun txt -> 
        if String.length txt.str <= txt.pos then Error ("Could match char '" ^ str_ch ^ "' as input string is empty")
        else if txt.str.[txt.pos] != ch then Ok (txt.str.[txt.pos], { txt with pos = txt.pos + 1 })
        else Error ("Coldn't match char '" ^ str_ch ^ "' with '"^ (char_to_string txt.str.[txt.pos]) ^"'")}


let pChoose (parsers:'a parser list) : 'a parser =
    let rec parse txt = function
    | [] -> Error ("None of parsers has succeeded: " ^ (parsers |> List.map (fun x->x.name) |> String.concat ", "))
    | p::ps -> match p.parse txt with Ok _ as o -> o | Error _ -> parse txt ps in
    { name = "any parser";
      parse = fun txt -> parse txt parsers; }

let (|||) a b =  pChoose [a;b]

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

let pZeroAll2 (parser:'a parser) : 'a list parser =
    let rec parse acc txt = 
        match parser.parse txt with
        | Ok (v, txt) -> parse (v::acc) txt
        | Error _ -> Ok (List.rev acc, txt)
    in
    { name = "any parser";
      parse = fun txt -> parse [] txt; }


let pCharRange startChar endChar =
    let startChar' = Char.code startChar in
    let endChar' = Char.code endChar in
    (List.init (endChar' - startChar') (fun ch -> Char.chr (startChar' + ch) |> pChar)
    |> pChoose)
    @@ "[" ^ (char_to_string startChar) ^ " .. " ^ (char_to_string endChar) ^ "]"
    |> pAll2


let chars_to_string x = String.concat "" (List.map (String.make 1) x)

let pString = pCharRange 'A' 'z' @=> chars_to_string

let pStringLiteral (quote:char) = pChar quote >>> (pAll2 ((pChar '\\' >>> pChar quote) ||| pNotChar quote) @=> chars_to_string) <<< pChar quote

let pStr str = (String.to_seq str |> List.of_seq |> List.map pChar |> pAll) @@ ("parse string " ^ str)
let pInt = 
    let numChars = (pCharRange '0' '9') in
    let parse = (pStr "0x" >>> numChars @=> fun b -> '0' :: 'x' :: b) ||| numChars in
    { name = "int parser";
      parse = fun txt -> 
            match parse.parse txt with
            | Ok (v, txt2) ->
                let v = chars_to_string v in begin
                match v |> Int64.of_string_opt with 
                | Some x -> Ok (x, txt2)
                | None -> Error ("cannot parse '" ^ v ^ "' to int") end
            | Error e -> Error e }



let pInteger = pInt

let pWhitespace = ['\r'; ' '; '\t'] |> List.map pChar |> pChoose |> pAll2

let pZeroWhitespace = { pWhitespace with parse = fun txt -> match pWhitespace.parse txt with Ok _ as o -> o | Error _ -> Ok ([], txt)  }

let refl (p: 'a parser -> 'a parser) : 'a parser =
    let r = ref (fun () -> failwith "impossible") in
    let z = p { name = "refl"; parse = fun txt -> (!r()).parse txt } in
    let x = fun () -> z in
    r := x;
    z

let pSep pWhat chSep =
    (pWhat <|> (pZeroAll2 (pZeroWhitespace >>> pChar chSep >>> pWhat))) @=> fun (x, xs) -> x :: xs

let pBetween chStart pWhat chSep chEnd = pChar chStart >>> pSep pWhat chSep <<< pChar chEnd

let pIgnore a = a @=> ignore