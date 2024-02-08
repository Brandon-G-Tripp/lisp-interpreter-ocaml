open Lib

let%test "_test simple lexer" = 
  let lexbuf = Lexing.from_string "1" in
  match Lexer.token lexbuf with
  | "1" -> true
  | _ -> false
