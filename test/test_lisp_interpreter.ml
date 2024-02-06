open OUnit2

let suite = "Test suite" >::: [
    "addition" >:: (fun _ -> 
        assert_equal 1 1 
    );
] 

let _ = 
    run_test_tt_main suite
