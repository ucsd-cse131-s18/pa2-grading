open Compile
open Runner
open Printf
open OUnit2
open Expr

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx"
let t_i name program expected args = name>::test_run program name expected args
let t name program expected = name>::test_run program name expected []
let terr_i name program expected args = name>::test_err program name expected args
let t_err name program expected = name>::test_err program name expected []
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (Runner.parse_string program));;
let t_f test_type = (fun (name,program,expected) ->
  test_type name program expected)
let f_to_s fname = Runner.string_of_file ("input/" ^ fname ^ ".boa")

let num_neg = "(+ -42 10)";;
let forty_one = "(sub1 42)";;
let forty = "(sub1 (sub1 42))";;
let add1 = "(add1 (add1 (add1 3)))";;
let def_x = "(let ((x 5)) x)";;
let def_x2 = "(let ((x 5)) (sub1 x))";;
let def_x3 = "(let ((x 5)) (let ((x 67)) (sub1 x)))";;
let def_x4 = "(let ((x (let ((x 5)) (sub1 x)))) (sub1 x))";;
let addnums = "(+ 5 10)";;
let nested_add = "(+ 5 (+ 10 20))";;
let nested_add2 = "(+ (- 10 5) 20)";;
let nested_arith = "(- (* (- 54 3) 2) 102)";;
let let_nested = "(let ((x (+ 5 (+ 10 20)))) (* x x))"
let complexExpression = "(let ((x 10) (y 5) (z 3)) (let ((t 2)) " ^
                        "(add1 (+ x (+ y (* (- t z) x))))))"
let ifTest = "(if true 5 6)"
let ifTestLet = "(let ((x 5)) (if (== x 7) 7 8))"
let boolTest = "true"
let isBoolTest = "(isBool false)"
let isBoolTestF = "(isBool 5)"
let isNumTest = "(isNum 5)"

(* Autograder test program string *)
let complexExpressionParens = "(let ((x 10) (y  5) (z 3)) (let (t 2) " ^
                              "(add1 (+ x (+ y (* (- t z) x))))))"
let deeparith = "(+ (* (- (* (+ 1 2) (* 2 4)) (+ -5 4)) (+ (* 12 3) 8)) 17)"
let lotOfLet = "(let ((x 1) (y x)) (let ((z y)) (let ((a (add1 z))) (sub1 a))))"
let quickBrownFox = "(let ((x (add1 (+ (* (- (* (sub1 55) (add1 17)) 33) " ^
                    "(sub1 -3)) (* -1 3)))) (y (sub1 (+ -17 x)))) y)"
let plusOne = "(let ((x 1) (y (add1 x)) (z (add1 y))) z)"
let plusOneScope = "(let ((x 1)) (let ((y (add1 x))) (let ((z (add1 y))) z)))"
let harmonic = "(sub1 (sub1 (add1 (add1 (sub1 (sub1 (add1 (* 1 (* 1 0)))))))))"
let complex_if = "(let ((x 6) (y (add1 x))) (if (> (+ 5 7) (* x y)) x y))"
let equality_check = "(== -1 -1)"
let nequality_check = "(== 5 6)"
let num_p_overflow = "1073741824"
let num_p_underflow = "-1073741825"

let forty_one_p = EPrim1(Sub1, ENumber(42));;
let forty_p = EPrim1(Sub1, EPrim1(Sub1, ENumber(42)));;
let add1_p = EPrim1(Add1, EPrim1(Add1, EPrim1(Add1, ENumber(3))));;
let def_x_p = ELet([("x", ENumber(5))], EId("x"));;
let def_x2_p = ELet([("x", ENumber(5))], EPrim1(Sub1, EId("x")));;
let def_x3_p = ELet([("x", ENumber(5))], ELet([("x", ENumber(67))],
               EPrim1(Sub1, EId("x"))));;
let def_x4_p = ELet([("x", ELet([("x", ENumber(5))], EPrim1(Sub1, EId("x"))))],
               EPrim1(Sub1, EId("x")));;
let addnums_p = EPrim2(Plus, ENumber(5), ENumber(10));;
let nested_add_p = EPrim2(Plus, ENumber(5),
                   EPrim2(Plus, ENumber(10), ENumber(20)));;
let nested_add2_p = EPrim2(Plus, EPrim2(Minus, ENumber(10), ENumber(5)),
                    ENumber(20));;
let nested_arith_p = EPrim2(Minus, EPrim2(Times, EPrim2(Minus, ENumber(54),
                     ENumber(3)), ENumber(2)), ENumber(102));;
let let_nested_p = ELet([("x", EPrim2(Plus, ENumber(5), EPrim2(Plus, ENumber(10),
                   ENumber(20))))], EPrim2(Times, EId("x"), EId("x")));;
let complexExpression_p = ELet([("x", ENumber(10)); ("y", ENumber(5));
                          ("z", ENumber(3))], ELet([("t", ENumber(2))],
                          EPrim1(Add1, EPrim2(Plus, EId("x"), EPrim2(Plus,
                          EId("y"), EPrim2(Times, EPrim2(Minus, EId("t"),
                          EId("z")), EId("x")))))));;
let complexExpressionParens_p = complexExpression_p;;
let deeparith_p = EPrim2(Plus, EPrim2(Times, EPrim2(Minus, EPrim2(Times,
                  EPrim2(Plus, ENumber(1), ENumber(2)),
                  EPrim2(Times, ENumber(2), ENumber(4))),
                  EPrim2(Plus, ENumber(-5), ENumber(4))),
                  EPrim2(Plus, EPrim2(Times, ENumber(12), ENumber(3)),
                  ENumber(8))), ENumber(17));;
let lotOfLet_p = ELet([("x", ENumber(1)); ("y", EId("x"))],
                 ELet([("z", EId("y"))], ELet([("a", EPrim1(Add1, EId("z")))],
                 EPrim1(Sub1, EId("a")))));;
let quickBrownFox_p = ELet([("x", EPrim1(Add1, EPrim2(Plus, EPrim2(Times,
                      EPrim2(Minus, EPrim2(Times, EPrim1(Sub1, ENumber(55)),
                      EPrim1(Add1, ENumber(17))), ENumber(33)),
                      EPrim1(Sub1, ENumber(-3))), EPrim2(Times, ENumber(-1),
                      ENumber(3))))); ("y", EPrim1(Sub1, EPrim2(Plus,
                      ENumber(-17), EId("x"))))], EId("y"));;

let autograde_parse_tests =
  [
    ("forty parse", forty, forty_p);
    ("add1 parse", add1, add1_p);
    ("def_x parse", def_x, def_x_p);
    ("def_x2 parse", def_x2, def_x2_p);
    ("def_x3 parse", def_x3, def_x3_p);
    ("def_x4 parse", def_x4, def_x4_p);
    ("addnums parse", addnums, addnums_p);
    ("nested_add parse", nested_add, nested_add_p);
    ("nested_add2 parse", nested_add2, nested_add2_p);
    ("nested_arith parse", nested_arith, nested_arith_p);
    ("let_nested parse", let_nested, let_nested_p);
    ("complexExpression parse", complexExpression, complexExpression_p);
    ("deeparith parse", deeparith, deeparith_p);
    ("lotOfLet parse", lotOfLet, lotOfLet_p);
    ("quickBrownFox parse", quickBrownFox, quickBrownFox_p);
  ]

let autograde_tests =
  [
    ("Num", "5", "5");
    ("Prim", "(add1 0)", "1");
    ("letTest_f", (f_to_s "letTest"), "6");
    ("manyOpTest_f", (f_to_s "manyOpTest"), "-52");
    ("deepArith", deeparith, "1117");
    ("lotOfLet", lotOfLet, "1");
    ("quickBrownFox", quickBrownFox, "-3776");
    ("plusOne", plusOne, "3");
    ("plusOneScope", plusOneScope, "3");
    ("harmonic", harmonic, "-1");
    ("input_t", (f_to_s "input"), "false");
    ("complex_if", complex_if, "7");
    ("equality_check", equality_check, "true");
    ("nequality_check", nequality_check, "false");
    ("lessThanTest", "(< 3 4)", "true");
    ("nlessThanTest", "(< 4 3)", "false");
    ("nlessThanTestEq", "(< 3 3)", "false");
    ("boolEvalonlyif", "(if (== 5 5) 2 (+ true false))", "2");
    ("boolEvalonlyelse", "(if (< 5 5) (+ true false) 3)", "3");
  ]

let autograde_fail_tests =
  [
    ("parse_let", "(let ((let 5)) let)", "Invalid or unexpected name let");
    ("complexExpressionParens parse", complexExpressionParens,
      "Error: invalid binding structure");
    ("let syntax", "(let ((x 1)) x x)", "Error: invalid let syntax");
    ("add1 args", "(add1 1 2)", "Error: Too many arguments to add1");
    ("sub1 args", "(sub1 2 1)", "Error: Too many arguments to sub1");
    ("+ syntax", "(+ 1 2 3)", "Error: invalid + syntax");
    ("- syntax", "(- 3 2 1)", "Error: invalid - syntax");
    ("* syntax", "(* 1 2 3)", "Error: invalid * syntax");
    ("invalid sexp", "()", "Error: unknown sexp ()");
    ("overflowAdd1", "(add1 1073741823)", "overflow");
    ("underflowSub1", "(sub1 -1073741824)", "overflow");
    ("overflowAdd", "(+ 1073741811 100000)", "overflow");
    ("overflowSub", "(- 1073741811 -100000)", "overflow");
    ("underFlowAdd", "(+ -1073741811 -100000)", "overflow");
    ("underFlowSub", "(- -1073741811 100000)", "overflow");
    ("overFlowTimes", "(* 1000000 100000)", "overflow");
    ("underflowTimes", "(* 1000000 -11230000)", "overflow");
    ("addBoolL", "(+ true -11230000)", "expected a number");
    ("addBoolR", "(+ 0 false)", "expected a number");
    ("subBoolL", "(- true -11230000)", "expected a number");
    ("subBoolR", "(- 0 false)", "expected a number");
    ("mulBoolL", "(* true -11230000)", "expected a number");
    ("mulBoolR", "(* 0 false)", "expected a number");
    ("lessBoolL", "(< true -11230000)", "expected a number");
    ("lessBoolR", "(< 0 false)", "expected a number");
    ("grtrBoolL", "(> true -11230000)", "expected a number");
    ("grtrBoolR", "(> 0 false)", "expected a number");
    ("eqExpectInt", "(== 0 false)", "expected a number");
    ("eqExpectBool", "(== false 0)", "expected a bool");
    ("lToREval", "(* (+ 0 false) (if 5 3 4))", "expected a number");
    ("boolTypeErr", "(if 5 2 3)", "expected a bool");
    ("boolTypeErrExpr", "(if (+ 5 7) 2 3)", "expected a bool");
    ("invalidID", "$ASD", "Invalid");
  ]

let failLet = "(let ((x  1) (y 1) (x 10)) x)"
let failID = "x"
let failTypes = "(add1 true)"

let testFailList =
  [
   t_err "failLet" failLet "Compile error: Multiple bindings for variable identifier x";
   t_err "failID" failID "Compile error: Variable identifier x unbound";
   t_err "failTypes" failTypes "expected a number";
   t_err "parserNumOverflow" num_p_overflow "Non-representable number"; 
   t_err "parserNumUnderflow" num_p_underflow "Non-representable number";
   terr_i "failInput" "input" "input must be a boolean or a number" ["0r"];
   terr_i "failInputType" "(add1 input)" "expected a number" ["true"];
  ]

let input_tests =
 [ t_i "input1" "input" "42" ["42"]
 ; t_i "input2" "input" "true" ["true"]
 ; t_i "input3" "input" "false" ["false"]
 ; t_i "input_default" "input" "false" []
 ; t_i "input_shadow" "(let ((input 10)) input)" "10" ["true"]

 ; terr_i "inputerr1" "input" "Error: input must be a boolean or a number" ["ABC"]
 ; terr_i "inputerr_max" "input" "Error: input is not a representable number" ["1073741824"]
 ; terr_i "inputerr_min" "input" "Error: input is not a representable number" ["-1073741825"]
 ; terr_i "inputerr_case" "input" "Error: input must be a boolean or a number" ["False"]
 ]

let suite =
  "suite">:::
  [t_parse "forty_one parse" forty_one forty_one_p;
  ] @
  [t "num_neg" num_neg "-32";
   t "forty_one" forty_one "41";
   t "forty" forty "40";
   t "add1" add1 "6";
   t "def_x" def_x "5";
   t "def_x2" def_x2 "4";
   t "def_x3" def_x3 "66";
   t "def_x4" def_x4 "3";
   t "addnums" addnums "15";
   t "nested_add" nested_add "35";
   t "nested_add2" nested_add2 "25";
   t "nested_arith" nested_arith "0";
   t "let_nested" let_nested "1225";
   t "complexExpression" complexExpression "6";
   t "boolTest" boolTest "true";
   t "if_Test" ifTest "5";
   t "ifTestLet" ifTestLet "8";
   t "isBoolTest" isBoolTest "true";
   t "isBoolTestF" isBoolTestF "false";
   t "isNumTest" isNumTest "true";
   t_i "inputTest" "(add1 input)" "6" ["5"];
  ] @ testFailList
  @ input_tests @ testFailList
  @ (List.map (t_f t_parse) autograde_parse_tests)
  @ (List.map (t_f t) autograde_tests)
  @ (List.map (t_f t_err) autograde_fail_tests)

let () =
  run_test_tt_main suite
;;
