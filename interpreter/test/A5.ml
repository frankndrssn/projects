open OUnit2
open Ruse.Ast
open Ruse.Interp
open Ruse.Parse

let parse_expr s =
  match expr_of_string Format.std_formatter s with
  | Some e -> e
  | None -> failwith ("parsing failed: " ^ s)

let sum =
  "(fun (sum (xs : (list int)) : int) (match xs (0) ((x ys) (+ x (sum ys)))))"

let assert_eq x y = fun _ -> assert_equal x y
let assert_some x = fun _ -> assert_bool "not some" (Option.is_some x)
let typeof_expr e = typeof empty_tenv (parse_expr e)
let interp_expr e = interpret empty_env (parse_expr e)

let tests = "Tests" >::: [
    (* well-typed stuff *)
    "typeof var" >:: assert_eq
      (Some Int)
      (typeof (add_type_binding "x" Int empty_tenv) (parse_expr "x"));
    "typeof 42" >:: assert_eq
      (Some Int)
      (typeof_expr "42");
      "typeof true" >:: assert_eq
      (Some Bool)
      (typeof_expr "true");
    "typeof false" >:: assert_eq
      (Some Bool)
      (typeof_expr "false");
    "typeof 42 + 24" >:: assert_eq
      (Some Int)
      (typeof_expr "(+ 42 24)");
    "typeof 42 - 24" >:: assert_eq
      (Some Int)
      (typeof_expr "(- 42 24)");
    "typeof 42 * 24" >:: assert_eq
      (Some Int)
      (typeof_expr "(* 42 24)");
    "typeof 42 < 24" >:: assert_eq
      (Some Bool)
      (typeof_expr "(< 42 24)");
    "typeof 42 = 24" >:: assert_eq
      (Some Bool)
      (typeof_expr "(= 42 24)");
    "typeof if" >:: assert_eq
      (Some Int)
      (typeof_expr "(if true 42 505)");
    "typeof app" >:: assert_eq
      (Some Int)
      (typeof_expr "((fun (id (x : int) : int) x) 42)");
    "typeof id" >:: assert_eq
      (Some (Arrow (Int, Int)))
      (typeof_expr "(fun (f (x : int) : int) x)");
    "typeof list" >:: assert_eq
      (Some (Lst Int))
      (typeof_expr "`[int]()");
    "typeof cons" >:: assert_eq
      (Some (Lst Int))
      (typeof_expr "(cons 42 `[int](505))");
    "typeof match" >:: assert_eq
      (Some Int)
      (typeof_expr "(match `[int](41 42 43) (123) ((x y) x))");
    "typeof pair" >:: assert_eq
      (Some (Prod (Int, Bool)))
      (typeof_expr "`(42 . true)");
    "typeof fst" >:: assert_eq
      (Some Int)
      (typeof_expr "(fst `(42 . true))");
    "typeof snd" >:: assert_eq
      (Some Bool)
      (typeof_expr "(snd `(42 . true))");
    "typeof inl" >:: assert_eq
      (Some (Either (Int, Bool)))
      (typeof_expr "(left 42 bool)");
    "typeof inr" >:: assert_eq
      (Some (Either (Int, Bool)))
      (typeof_expr "(right int true)");
    "typeof case" >:: assert_eq
      (Some Int)
      (typeof_expr "(case (left 42 bool) ((x) x) ((y) 84))")
  ] @ [
      (* ill-typed stuff *)
      "typeof 42 true" >:: assert_eq
        None
        (typeof_expr "(42 true)");
      "typeof `[int](true)" >:: assert_eq
        None
        (typeof_expr "`[int](true)");
      "typeof 42 + true" >:: assert_eq
        None
        (typeof_expr "(+ 42 true)")
    ] @ [
      (* interpret stuff *)
      (* [value] is abstract so the outputs are tested interactively *)
      "interp add" >:: assert_some
        (interp_expr "(+ 42 24)");
      "interp sum [1, 2, 3]" >:: assert_some
        (interp_expr @@ "(" ^ sum ^ " `[int](1 2 3))")
    ]

let () = run_test_tt_main tests
