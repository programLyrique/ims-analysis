open OUnit2

let suite = "tests" >::: [Dowsampling_tests.suite ; Audiograph_parser_tests.suite]

let () =
  run_test_tt_main suite
