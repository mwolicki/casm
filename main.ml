open Parser
open Tokenizer
open Stdint

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

let size_to_int = function `BYTE -> 1 | `WORD -> 2 | `DWORD -> 4 | `QWORD -> 8

let emit_data size = 
    let to_bytes (v:Int64.t) = 
        let buff = Bytes.create (size_to_int size) in
        let () =
            match size with 
            | `BYTE -> Int8.to_bytes_little_endian (Int8.of_int64 v) buff 0
            | `WORD -> Int16.to_bytes_little_endian (Int16.of_int64 v) buff 0
            | `DWORD -> Int32.to_bytes_little_endian (Int32.of_int64 v) buff 0
            | `QWORD -> Int64.to_bytes_little_endian (Int64.of_int64 v) buff 0 in
        buff in

    let rec emit_data (acc:Bytes.t list) = function 
        | [] -> List.rev acc |> List.fold_left Bytes.cat Bytes.empty
        | Ident x :: xs -> 
            let x = Bytes.of_string x in
            emit_data (x::acc) xs
        | Numb x :: xs -> let x = to_bytes x in emit_data (x::acc) xs in
    emit_data []

let emit = function 
| Op ("db", args) -> emit_data `BYTE args
| Op ("dw", args) -> emit_data `WORD args
| Op ("dd", args) -> emit_data `DWORD args
| Op ("dq", args) -> emit_data `QWORD args
| Directive _ -> (*Ignore for now *) Bytes.empty
| Label _ -> (*Ignore for now *) Bytes.empty
| x -> failwith ("Unknown ast element " ^ (ast_to_string x))

let () =
    let o = load_file "test.asm" in
    
    match o |> Parser.to_txt |> pFile.parse with
    | Ok (ast, { pos = pos; _ }) when pos = String.length o ->
        let t = List.map ast_to_string ast in
        "Yay " ^ String.concat "\n" t |> print_endline;
        let bytes = List.map emit ast |> List.fold_left Bytes.cat Bytes.empty in
        let o = open_out_bin "test.o" in
        output_bytes o bytes;
        close_out o

    | Ok (_, txt) -> 
        "noooop; pos" ^ (pos_to_string txt) |> print_endline;
        exit 1
    | Error r -> 
        "noooop " ^ r |> print_endline;
        exit 2
