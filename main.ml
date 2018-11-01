let is = function
| Ok (i,_) -> "YAY " ^ (i) 
| Error r -> "noooop " ^ r


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

let line = 
    (pZeroWhitespace >>> pString <|> pAll2 (pWhitespace >>> pString) <<< pZeroWhitespace) @=> fun (a, x)-> a ^" " ^ String.concat "<->" x 

let () =
    let o = load_file "test.asm" in
    
    String.split_on_char '\n' o
    |> List.map (fun x->Parser.to_txt x |> line.parse )
    |> List.iter (fun z->is z |> print_endline)