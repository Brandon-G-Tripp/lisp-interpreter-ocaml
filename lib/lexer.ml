open Char 

type token = 
    | LeftParen
    | RightParen
    | Number of int
    | Symbol of string

let lex (str: string) : token list = 
    let rec loop index tokens = 
        match String.get str index with 
        | '(' -> LeftParen :: loop (index + 1) tokens
        | ')' -> RightParen :: loop (index + 1) tokens
        | char when Char.is_digit char -> 
                let (token, next_index) = scan_number str index in
                token :: loop next_index tokens
        | char when is_symbol_char char ->
                let (token, next_index) = scan_symbol str index in
                token :: loop next_index tokens
        | _ -> failwith "Unimplemented token type"
    in
    loop 0 []


let rec scan_number str index = 
    if index >= String.length str then
        failwith "Unexpected end of input"
    else
        let char = String.get str index in 
        if Char.is_digit char then 
            let (token, next_index) = scan_number str (index + 1) in
            (Number (int_of_string (char :: token)), next_index)
        else
            (Number (int_of_string (String.of_list (List.rev [char]))), index)

let is_symbol_char char = 
    match char with 
    | 'a'..'z' | 'A'..'Z' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '~' | '_' | '+' | '-' ->
        true
    | _ -> false

let rec scan_symbol str index = 
    if index >= String.length then 
        failwith "Unexpected end of input"
    else
        let char = String.get str index in 
        if is_symbol_char char then 
            let (token, next_index) = scan_symbol str (index + 1) in
            (Symbol (char :: token), next_index)
        else
            (Symbol (String.of_list (List.rev [])), index) 


