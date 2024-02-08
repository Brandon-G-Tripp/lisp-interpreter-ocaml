open Lexer

let simple_test() = 
    let input = "(+ 2 x)" in
    let expected = [LeftParen; Symbol "+"; Number 2; Symbol "x"; RightParen] in 
    let actual = Lexer.lex input in 
    assert_equal expected actual
