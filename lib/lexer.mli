type token = 
    | LeftParen
    | RightParen
    | Number of int
    | Symbol of string

val lex : string -> token list
