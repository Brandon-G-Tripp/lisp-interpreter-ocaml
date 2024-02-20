type token = 
    | LeftParen
    | RightParen
    | Number of int
    | Symbol of string
    | String of string

let is_digit c = 
    '0' <= c && c <= '9'

let is_symbol_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let next_char str index = 
    str.[index]

let peek_char str index = 
    if index < String.length str then 
        Some str.[index]
    else
        None

let rec scan_number buf str index = 
    match peek_char str index with
    | Some c when is_digit c -> 
            let buf' = buf ^ String.make 1 c in
            scan_number buf' str (index + 1)
    | _ -> 
            Number(int_of_string buf), index


let rec scan_symbol buf str index = 
    match peek_char str index with
    | Some c when is_symbol_char c -> 
            let buf' = buf ^ String.make 1 c in
            scan_symbol buf' str (index + 1)
    | _ -> 
            Symbol buf, index

let lex str = 
    let rec loop index tokens = 
        match peek_char str index with
        | Some '(' -> loop (index + 1) (LeftParen :: tokens)
        | Some ')' -> loop (index + 1) (RightParen :: tokens)
        | Some c when is_digit c -> 
                let number, next_index = scan_number (String.make 1 c) str index in
                loop next_index (number :: tokens)
        | Some c when is_symbol_char c -> 
                let symbol, next_index = scan_symbol (String.make 1 c) str index in
                loop next_index (symbol :: tokens)
        | _ -> tokens
    in
    loop 0 []

