let is = function
| Ok (i,_) -> "YAY " ^ (string_of_int i) 
| Error r -> "noooop " ^ r

let () =
    ["0";"11";"0x123";"1";]
    |> List.map (fun x->Parser.to_txt x |> (Parser.pInteger).parse )
    |> List.iter (fun z->is z |> print_endline)
