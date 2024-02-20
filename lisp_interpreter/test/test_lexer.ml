open Lib
open Alcotest

let test_simple_lexer () = 
      let tokens = lex "1" in
      Alcotest.(check (list token)) "Single number" tokens [Number 1]

let test_composite_lexer () = 
    let tokens = lex "(+ 2 3)" in
    Alcotest.(check (list token))
        "Expression with numbers and symbols" tokens
        [LeftParen; Symbol "+"; Number 2; Number 3; RightParen]

let tests = [
    "Suite1", [
        test_case "Simple lexer" `Quick test_simple_lexer;
        test_case "Composite lexer" `Quick test_composite_lexer;
    ];
]

let () = 
    Alcotest.run "Test Suite" tests

