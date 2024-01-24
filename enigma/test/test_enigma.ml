open OUnit2
open Enigma

(* Historical rotors *)
let rotor_I = { wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ" ; turnover = 'Q' }
let rotor_II = { wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE" ; turnover = 'E' }
let rotor_III = { wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO" ; turnover = 'V' }
let rotor_IV = { wiring = "ESOVPZJAYQUIRHXLNFTGKDCMWB" ; turnover = 'J' }

(* New rotor *)
let rotor_C = { wiring = "NFYPBMRUZCIOJVWTKLQSXHAEGD" ; turnover = 'A' }

(* Historical reflectors *)
let refl_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let refl_C = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

(* Plugboards *)
let full_plugboard = [
  ('A','B'); ('C', 'D'); ('E', 'F'); ('G', 'H'); ('I', 'J');
  ('K', 'L'); ('M', 'N'); ('O', 'P'); ('Q', 'R'); ('S', 'T');
  ('U', 'V'); ('W', 'X'); ('Y', 'Z')
]

let half_plugboard = [
  ('A','B'); ('C', 'D'); ('I', 'J'); ('K', 'L');
  ('M', 'N'); ('O', 'P'); ('Q', 'R')
]

let reordered_plugboard = [
  ('A','B'); ('O', 'P'); ('R', 'Q'); ('I', 'J');
  ('K', 'L'); ('M', 'N'); ('C', 'D')
]

(* index *)
let tests = "index" >::: [
    "index of A" >:: (fun _ -> assert_equal 0 (index 'A'));
    "index of F" >:: (fun _ -> assert_equal 5 (index 'F'));
    "index of Z" >:: (fun _ -> assert_equal 25 (index 'Z'))
  ]

let _ = run_test_tt_main tests
(* *************** *)

(* map_r_to_l
 * identity: an extremal case where the function is just the identity.
 * offset_1, offset_2: test whether offset works correctly.
 * large_offset: a large offset causes the computed value to be outside the
 *               range of 0..25, it makes sure that (%) is implemented
 *               correctly.
*)

let tests = "map_r_to_l" >::: [
    "identity" >:: (fun _ -> assert_equal 7
                       (map_r_to_l "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 7));
    "offset_1" >:: (fun _ -> assert_equal 4
                       (map_r_to_l "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0));
    "offset_2" >:: (fun _ -> assert_equal 9
                       (map_r_to_l "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0));
    "large_offset" >:: (fun _ -> assert_equal 17
                           (map_r_to_l "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O' 14))
  ]

let _ = run_test_tt_main tests
(* *************** *)


(* map_l_to_r
 * simple_input_*: simple inputs that were bugged on the first try.
 * offset_1, offset_2: test whether offset works correctly.
 * large_offset: a large offset causes the computed value to be outside the
 *               range of 0..25, it makes sure that (%) is implemented
 *               correctly.
*)

let tests = "map_l_to_r" >::: [
    "simple_input_1" >:: (fun _ -> assert_equal 20
                             (map_l_to_r "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0));
    "simple_input_2" >:: (fun _ -> assert_equal 21
                             (map_l_to_r "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0));
    "simple_input_3" >:: (fun _ -> assert_equal 14
                             (map_l_to_r "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10));
    "offset_1" >:: (fun _ -> assert_equal 1
                       (map_l_to_r "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0));
    "offset_2" >:: (fun _ -> assert_equal 0
                       (map_l_to_r "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 0));
    "large_offset" >:: (fun _ -> assert_equal 14
                           (map_l_to_r "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'Z' 14))
  ]

let _ = run_test_tt_main tests
(* *************** *)

(* map_refl
 * simple_input_*: sanity check.
*)
let tests = "map_refl" >::: [
    "simple_input_1" >:: (fun _ -> assert_equal 21
                             (map_refl "ZYXWVUTSRQPONMLKJIHGFEDCBA" 4));
    "simple_input_2" >:: (fun _ -> assert_equal 4
                             (map_refl "ZYXWVUTSRQPONMLKJIHGFEDCBA" 21));
    "simple_input_3" >:: (fun _ -> assert_equal 24
                             (map_refl "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0));
    "simple_input_4" >:: (fun _ -> assert_equal 0
                             (map_refl "YRUHQSLDPXNGOKMIEBFZCWVJAT" 24))
  ]

let _ = run_test_tt_main tests
(* *************** *)

(* map_plug
 * identity: extremal case where the function should just be the identity.
 * full_1, full_2: extremal case where the plugboard is full.
 * no_plug: makes sure that the function behaves like the identity for letters
 *          without a plug in a non-full plugboard.
 * has_plug: makes sure that the letter is swapped because it has a plug.
 * order: makes sure that the order in which the plugboard is presented does
 *        not matter.
*)
let tests = "map_plug" >::: [
    "identity" >:: (fun _ -> assert_equal 'F' (map_plug [] 'F'));
    "full_1" >:: (fun _ -> assert_equal 'K' (map_plug full_plugboard 'L'));
    "full_2" >:: (fun _ -> assert_equal 'L' (map_plug full_plugboard 'K'));
    "no_plug" >:: (fun _ -> assert_equal 'E' (map_plug half_plugboard 'E'));
    "has_plug" >:: (fun _ -> assert_equal 'Q' (map_plug half_plugboard 'R'));
    "order" >:: (fun _ -> assert_equal 'Q' (map_plug reordered_plugboard 'R'));
  ]

let _ = run_test_tt_main tests
(* *************** *)

let simple_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_I ; top_letter = 'A' };
    { rotor = rotor_II ; top_letter = 'A' };
    { rotor = rotor_III ; top_letter = 'A' }
  ];
  plugboard = []
}

let offset_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'K' };
    { rotor = rotor_II ; top_letter = 'D' };
    { rotor = rotor_I ; top_letter = 'O' }
  ];
  plugboard = []
}

let no_rotors = {
  refl = refl_B;
  rotors = [];
  plugboard = half_plugboard
}

let two_rotors = {
  refl = refl_C;
  rotors = [
    { rotor = rotor_III ; top_letter = 'B' };
    { rotor = rotor_C ; top_letter = 'C' };
  ];
  plugboard = half_plugboard
}

(* cipher_char
 * simple_input: sanity check.
 * no_rotors_1, no_rotors_2: machine with no rotors, this should only take the
 *                           plugboard and the reflector into account.
 * two_rotors: makes sure that the function is robust under non-standard config.
*)
let tests = "cipher_char" >::: [
    "simple_input" >:: (fun _ -> assert_equal 'P'
                           (cipher_char simple_config 'G'));
    "no_rotors_1" >:: (fun _ -> assert_equal 'S'
                          (cipher_char no_rotors 'F'));
    "no_rotors_2" >:: (fun _ -> assert_equal 'G'
                          (cipher_char no_rotors 'K'));
    "two_rotors" >:: (fun _ -> assert_equal 'R'
                         (cipher_char two_rotors 'K'))
  ]

let _ = run_test_tt_main tests
(* *************** *)

let step_1_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'K' };
    { rotor = rotor_II ; top_letter = 'D' };
    { rotor = rotor_I ; top_letter = 'P' }
  ];
  plugboard = []
}

let step_2_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'K' };
    { rotor = rotor_II ; top_letter = 'D' };
    { rotor = rotor_I ; top_letter = 'Q' }
  ];
  plugboard = []
}

let step_3_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'K' };
    { rotor = rotor_II ; top_letter = 'E' };
    { rotor = rotor_I ; top_letter = 'R' }
  ];
  plugboard = []
}

let step_4_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'L' };
    { rotor = rotor_II ; top_letter = 'F' };
    { rotor = rotor_I ; top_letter = 'S' }
  ];
  plugboard = []
}

let no_rotors = {
  refl = refl_C;
  rotors = [];
  plugboard = full_plugboard
}

let more_rotors = {
  refl = refl_C;
  rotors = [
    { rotor = rotor_III ; top_letter = 'V' };
    { rotor = rotor_C ; top_letter = 'A' };
    { rotor = rotor_I ; top_letter = 'Q' };
    { rotor = rotor_III ; top_letter = 'V' };
    { rotor = rotor_II ; top_letter = 'E' };
  ];
  plugboard = full_plugboard
}

let more_step_1 = {
  refl = refl_C;
  rotors = [
    { rotor = rotor_III ; top_letter = 'W' };
    { rotor = rotor_C ; top_letter = 'B' };
    { rotor = rotor_I ; top_letter = 'R' };
    { rotor = rotor_III ; top_letter = 'W' };
    { rotor = rotor_II ; top_letter = 'F' };
  ];
  plugboard = full_plugboard
}

(* step
 * no_rotor: extremal case where there is no rotor so there's nothing to do.
 * step_*: captures an example in the assignment markdown.
 * more: makes sure that no rotor takes more than one step, every rotor
 *       satisfies the criteria to rotate.
*)
let tests = "step" >::: [
    "no_rotor" >:: (fun _ -> assert_equal no_rotors (step no_rotors));
    "step_1" >:: (fun _ -> assert_equal step_1_config (step offset_config));
    "step_2" >:: (fun _ -> assert_equal step_2_config (step step_1_config));
    "step_3" >:: (fun _ -> assert_equal step_3_config (step step_2_config));
    "step_4" >:: (fun _ -> assert_equal step_4_config (step step_3_config));
    "more" >:: (fun _ -> assert_equal more_step_1 (step more_rotors));
  ]

let _ = run_test_tt_main tests
(* *************** *)

let ocaml_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_I ; top_letter = 'F' };
    { rotor = rotor_II ; top_letter = 'U' };
    { rotor = rotor_III ; top_letter = 'N' };
  ];
  plugboard = [('A','Z')]
}

let wetterbericht_config = {
  refl = refl_C;
  rotors = [
    { rotor = rotor_IV ; top_letter = 'S' };
    { rotor = rotor_II ; top_letter = 'A' };
    { rotor = rotor_III ; top_letter = 'D' };
  ];
  plugboard = [
    ('C', 'K') ; ('V', 'W')
  ]
}

let deustchland_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'V' };
    { rotor = rotor_II ; top_letter = 'D' };
    { rotor = rotor_III ; top_letter = 'V' };
  ];
  plugboard = []
}

let deustchlandlied_config = {
  refl = refl_B;
  rotors = [
    { rotor = rotor_III ; top_letter = 'A' };
    { rotor = rotor_II ; top_letter = 'A' };
    { rotor = rotor_I ; top_letter = 'A' };
  ];
  plugboard = []
}

let input =
  "VQLXAMHZMEFTHNEKVBBNMEXABJSGAOTWRVBLGYYLOAYKWNTRTEVZECVBWQVZEEY\
   MURHRHVXCCYBADYTOSFSMELYKXWPGDJBFRVICJLIGRCWWRVCQTNSDPKPPHIDTSV\
   ZNJNQCIZXUQPIRUTLLRGQHYLRFNRBDRPFHHJCJAYVVIJECMGJGGKEJKXYVAUJJL\
   PFFXOIHSOXA"

let expected =
  "EINIGKEITUNDRECHTUNDFREIHEITFURDASDEUTSCHEVATERLANDDANACHLASSTUN\
   SALLESTREBENBRUDERLICHMITHERZUNDHANDEINIGKEITUNDRECHTUNDFREIHEIT\
   SINDDESGLUCKESUNTERPFANDBLUHIMGLANZEDIESESGLUCKESBLUHEDEUTSCHESV\
   ATERLAND"

(* cipher
 * empty: an extremal case where the string is empty. Nothing should be done.
 * ocaml: a suggested test case.
 * wetterbericht: weather report in German. Commonly tranmitted message by the
                  Nazi.
 * deustchland: causes every rotor to rotate. Robustness test.
 * deustchlandlied: a very long string. Causes some rotors to make more than
                    one full rotations. Robustness test.
*)
let tests = "cipher" >::: [
    "empty" >:: (fun _ -> assert_equal "" (cipher ocaml_config ""));
    "ocaml" >:: (fun _ -> assert_equal "OCAML" (cipher ocaml_config "YNGXQ"));
    "wetterbericht" >:: (fun _ -> assert_equal "WETTERBERICHT"
                            (cipher wetterbericht_config "DPDHYMMXJSXMA"));
    "deustchland" >:: (fun _ -> assert_equal "DEUSTCHLAND"
                          (cipher deustchland_config "OWEWROITXDV"));
    "deustchlandlied" >:: (fun _ -> assert_equal expected
                              (cipher deustchlandlied_config input));
  ]

let _ = run_test_tt_main tests
(* *************** *)

(*************************************************)
(*              Utility Functions                *)
(*************************************************)

let rec zero_through' (n : int) : int list =
  if n < 0
  then []
  else n :: zero_through' (n -1)

let zero_through n = List.rev (zero_through' n)

let all_indices = zero_through 25

let unindex (code : int) : char = Char.chr (code + Char.code 'A')

let all_chars = List.map unindex all_indices

let rec zip xs ys =
  match xs, ys with
  | [], [] -> Some []
  | [], _ -> None
  | _, [] -> None
  | (x :: xs), (y :: ys) -> 
    begin
      match zip xs ys with
      | Some zs -> Some ((x,y) :: zs)
      | None -> None
    end

let string_of_list (cs : char list) : string =
  String.init (List.length cs) (List.nth cs)

(*************************************************)
(*             Given Test Cases                  *)
(*************************************************)

(*************************************************)
(*               Example Inputs                  *)
(*************************************************)

let all_index_pairs = 
  match zip all_indices all_chars with 
  | Some xs -> xs
  | None -> failwith ("Not enough indices or characters")

let reflectorB : string = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let identity_reflector = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let identity_rotor_wiring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI_wiring : string = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII_wiring : string = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII_wiring : string = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let rotorI : rotor = {wiring = rotorI_wiring; turnover = 'Q'}
let rotorII : rotor = {wiring = rotorII_wiring; turnover = 'E'}
let rotorIII : rotor = {wiring = rotorIII_wiring; turnover = 'V'}

let config1 : config = {
  refl = reflectorB;
  rotors = [{rotor=rotorI; top_letter='A'};
            {rotor=rotorII; top_letter='A'};
            {rotor=rotorIII; top_letter='A'}];
  plugboard=[]
}
let config2 : config = {
  refl = reflectorB;
  rotors = [{rotor=rotorIII; top_letter='K'};
            {rotor=rotorII; top_letter='D'};
            {rotor=rotorI; top_letter='O'}];
  plugboard=[]
}
let config3 : config = {
  refl=reflectorB;
  rotors=[{rotor=rotorI; top_letter='F'};
          {rotor=rotorII; top_letter='U'};
          {rotor=rotorIII; top_letter='N'}];
  plugboard=[('A','Z')]
}

let config1_char_cipher_map =
  [
    ('A', 'U');
    ('B', 'E');
    ('C', 'J');
    ('D', 'O');
    ('E', 'B');
    ('F', 'T');
    ('G', 'P');
    ('H', 'Z');
    ('I', 'W');
    ('J', 'C');
    ('K', 'N');
    ('L', 'S');
    ('M', 'R');
    ('N', 'K');
    ('O', 'D');
    ('P', 'G');
    ('Q', 'V');
    ('R', 'M');
    ('S', 'L');
    ('T', 'F');
    ('U', 'A');
    ('V', 'Q');
    ('W', 'I');
    ('X', 'Y');
    ('Y', 'X');
    ('Z', 'H')
  ]

let config1_cipher_tests =
  [
    ("Types", "TYPES", "OMSWB");
    ("Record", "RECORD", "VLEMEV");
    ("Ocaml", "OCAML", "TRZOU");
    ("Buffalo", "BUFFALO", "AVUTOIM");
    ("Syracuse", "SYRACUSE", "JMXGNPVY")
  ]

let config2_cipher_tests = 
  [
    ("Ocaml", "VOMUZ", "OCAML");
    ("Buffalo", "BUFFALO", "ENSJJSV");
    ("Scala", "SCALA", "FOMDJ");
    ("Lambda", "LAMBDA", "NLANYC");
    ("Function", "FUNCTION", "SNQTCRVH")
  ]

let config3_cipher_tests =
  [
    ("Ocaml", "YNGXQ", "OCAML");
    ("Haskell", "HASKELL", "RLVCHZI");
    ("Scheme", "SCHEME", "JNFYJX");
    ("Racket", "RACKET", "HLUCHU");
    ("Scala", "SCALA", "JNGUZ")
  ]

let index_tests = "index tests" >:::
                  List.map (fun (i,c) ->
                      ("index " ^ (String.make 1 c) ^ " = " ^ (string_of_int i))
                      >:: (fun _ -> assert_equal (index c) i
                              ~printer:string_of_int))
                    all_index_pairs


let identity_rotor_wiring_tests = 
  List.concat_map 
    (fun i -> List.concat_map
        (fun c ->
           [(("Identity Rotor Test R to L " ^ (string_of_int i) ^ " " ^
              (String.make 1 c)) >:: 
             fun _ -> assert_equal i (map_r_to_l identity_rotor_wiring c i)
                 ~printer:string_of_int);
            (("Identity Rotor Test L to R " ^ (string_of_int i) ^ " " ^
              (String.make 1 c)) >::
             fun _ -> assert_equal i (map_l_to_r identity_rotor_wiring c i)
                 ~printer:string_of_int)]) 
        all_chars) 
    all_indices

let rotor_tests = "Rotor Tests" >:::
                  [
                    "Example 1" >:: (fun _ ->
                        assert_equal 4 (map_r_to_l
                                          "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0));
                    "Example 2" >:: (fun _ ->
                        assert_equal 9 (map_r_to_l
                                          "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0));
                    "Example 3" >:: (fun _ ->
                        assert_equal 20 (map_l_to_r
                                           "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0));
                    "Example 4" >:: (fun _ ->
                        assert_equal 21 (map_l_to_r
                                           "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0));
                  ] @ identity_rotor_wiring_tests

let full_reflector_test (refl: string) =
  List.map (fun i -> ("Reflector " ^ refl ^ " index " ^ (string_of_int i) >:: 
                      (fun _ -> assert_equal (i |> String.get refl |> index) 
                          (map_refl refl i) 
                          ~printer:string_of_int)))
    (zero_through 25)


let identity_reflector_tests =
  List.map (fun x -> ("Identity Spec " ^ (string_of_int x) >:: (fun _ ->
      assert_equal x (map_refl identity_reflector x) ~printer:string_of_int)))
    (zero_through 25)

let reflector_tests = "Reflector Tests" >::: 
                      [
                        "Example 1" >:: (fun _ ->
                            assert_equal 4 (map_refl
                                              "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0));
                      ] 
                      @ identity_reflector_tests
                      @ full_reflector_test reflectorB

let identity_plugboard_tests =
  List.map (fun c -> ("Empty Plugboard " ^ (String.make 1 c) >:: (fun _ ->
      assert_equal c (map_plug [] c) ~printer:(String.make 1)))) all_chars

let plugboard_tests = "Plugboard Tests" >::: identity_plugboard_tests

let example_1_tests = 
  List.map (fun (c1, c2) ->
      ("Example 1 " ^ (String.make 1 c1) ^ " " ^ (String.make 1 c2)) >::
      (fun _ ->
         assert_equal c1 (cipher_char config1 c2) ~printer:(String.make 1)))
    config1_char_cipher_map

let character_tests = "Cipher Character Tests" >::: example_1_tests

let assert_rotors (rs : string) (conf : config) = assert_equal rs
    (string_of_list (List.map (fun x -> x.top_letter) conf.rotors))
    ~printer:(fun x -> x)

let test_steps (test_name : string) =
  let rec go n rss config = match rss with 
    | [] -> []
    | (rs :: rss) -> ((test_name ^ ": " ^ "Step " ^ (string_of_int n)) >::
                      (fun _ -> assert_rotors rs config)) :: go (n+1) rss
                       (step config)
  in go 0

let example1_step_tests = ["KDO"; "KDP"; "KDQ"; "KER"; "LFS"; "LFT"; "LFU"]

let step_tests = "Step tests" >:::
                 test_steps "Example 1" example1_step_tests config2

let test_cipher (conf : config) (base_name, s1, s2) = 
  [ 
    (base_name ^ " Test 1") >:: (fun _ -> assert_equal s2 (cipher conf s1)
                                    ~printer:(fun x -> x));
    (base_name ^ " Test 2") >:: (fun _ -> assert_equal s1 (cipher conf s2)
                                    ~printer:(fun x -> x))
  ]

let test_ciphers (conf : config) (base_name : string)
    (tests : (string * string * string) list) = 
  tests |> 
  List.map (fun (n, x, y) -> ("Testing " ^ base_name ^ " (" ^ n ^ ")", x, y)) |> 
  List.concat_map (test_cipher conf)

let cipher_tests = "Cipher Tests" >:::
                   test_ciphers config1 "config1" config1_cipher_tests 
                   @ test_ciphers config2 "config2" config2_cipher_tests
                   @ test_ciphers config3 "config3" config3_cipher_tests

let _ = run_test_tt_main index_tests; 
  run_test_tt_main rotor_tests;
  run_test_tt_main reflector_tests;
  run_test_tt_main plugboard_tests; 
  run_test_tt_main character_tests;
  run_test_tt_main step_tests; 
  run_test_tt_main cipher_tests
