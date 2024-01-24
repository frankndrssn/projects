(* Approach:
 *   step:
 *     I aimed to provide some sort of separation of concern: rule 1, rule 2,
 *     and rule 3 should be considered separately. I achieved this by borrowing
 *     the idea similar to dirac delta function. Each rotor in the spindle that
 *     meets the criteria to rotate is expressed by a delta function
 *     delta : int -> bool
 *     a linear combination of these functions indicates which rotors need to
 *     be rotated. As it turns out, it is sufficient to compute just rule 2
 *     because we get the other 2 rules for free. *)

(**
   [index c] is the 0-based index of [c] in the alphabet.
   requires: [c] is an uppercase letter in A..Z
*)
let index (c : char) : int =
  let i = Char.code c in
  assert (65 <= i && i <= 90);
  i - 65

(**
   [index_inv] is the inverse of [index].
   requires: [i] is in 0..25
*)
let index_inv (i : int) : char =
  assert (0 <= i && i <= 25);
  Char.chr (i + 65)

(** [a % b] is the remainder of [a / b] based on Euclid's division algorithm.
    raises: [Division_by_zero]
*)
let (%) (a : int) (b : int) : int =
  let c = a mod b in
  if c < 0 then b + c else c

(**
   [offset_index i offset] is a descriptive name for the expression
   [(i + offset) % 26].
*)
let offset_index (i : int) (offset : int) : int =
  (i + offset) % 26

(** [next_char c] is the next character of [c].
    The next character of Z is A.
    requires: [c] is an uppercase letter in A..Z
*)
let next_char (c : char) : char =
  index_inv @@ offset_index (index c) 1

(**
   [map_r_to_l wiring top_letter input_pos] is the left-hand output position
   at which current would appear when current enters at right-hand input
   position [input_pos] to a rotor whose wiring specification is given by
   [wiring].  The orientation of the rotor is given by [top_letter],
   which is the top letter appearing to the operator in the rotor's
   present orientation.
   requires:
   - [wiring] is a valid wiring specification.
   - [top_letter] is in 'A'..'Z'
   - [input_pos] is in 0..25
*)
let map_r_to_l (wiring : string) (top_letter : char) (input_pos : int) : int =
  assert (0 <= input_pos && input_pos <= 25);
  let offset = index top_letter in
  let r_out = offset_index input_pos offset in
  let l_in = index wiring.[r_out] in
  offset_index l_in (-offset)

(**
   [w_inv wiring] is the inverse of the permutation specified by [wiring].
   requires:
   - [wiring] is a valid wiring specification.
   - [i] is in 0..25
*)
let w_inv (wiring : string) (i : int) : int =
  let c = index_inv i in
  String.index wiring c

(** [map_l_to_r] computes the same function as [map_r_to_l], except
    for current flowing left to right.
    requires:
    - [wiring] is a valid wiring specification.
    - [top_letter] is in 'A'..'Z'
    - [input_pos] is in 0..25
*)
let map_l_to_r (wiring : string) (top_letter : char) (input_pos : int) : int =
  assert (0 <= input_pos && input_pos <= 25);
  let offset = index top_letter in
  let l_out = offset_index input_pos offset in
  let r_in = w_inv wiring l_out in
  offset_index r_in (-offset)

(**
   [map_refl wiring input_pos] is the output position at which current would
   appear when current enters at input position [input_pos] to a reflector
   whose wiring specification is given by [wiring].
   requires:
   - [wiring] is a valid reflector specification.
   - [input_pos] is in 0..25
*)
let map_refl (wiring : string) (input_pos : int) : int =
  assert (0 <= input_pos && input_pos <= 25);
  index wiring.[input_pos]

(**
   [map_plug plugs c] is the letter to which [c] is transformed
   by the plugboard [plugs].
   requires:
   - [plugs] is a valid plugboard
   - [c] is in 'A'..'Z'
*)
let rec map_plug (plugs : (char * char) list) (c : char) : char =
  match plugs with
  | [] -> c
  | (a, b) :: tl ->
    begin
      if c = a then
        b
      else if c = b then
        a
      else
        map_plug tl c
    end

type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

(**
   [map_rotors_r_to_l spindle input_pos] simulates how current flows from right
   to left.
   requires:
   - spindle is valid
   - input_pos is in 0..25
*)
let map_rotors_r_to_l (spindle : oriented_rotor list) (input_pos : int)
  : int =
  let r_to_l ({ rotor ; top_letter } : oriented_rotor) (acc : int) =
    map_r_to_l rotor.wiring top_letter acc
  in
  List.fold_right r_to_l spindle input_pos

(**
   [map_rotors_l_to_r] is the same as [map_rotors_r_to_l] but the current flows
   in the opposite direction
   requires:
   - spindle is valid
   - input_pos is in 0..25
*)
let map_rotors_l_to_r (spindle : oriented_rotor list) (input_pos : int)
  : int =
  let l_to_r (acc : int) ({ rotor ; top_letter } : oriented_rotor) =
    map_l_to_r rotor.wiring top_letter acc
  in
  List.fold_left l_to_r input_pos spindle

(**
   [cipher_char config c] is the letter to which the Enigma machine
   ciphers input [c] when it is in configuration [config].
   requires:
   - [config] is a valid configuration
   - [c] is in 'A'..'Z'
*)
let cipher_char (config : config) (c : char) : char =
  let spindle_in_r_to_l = index @@ map_plug config.plugboard c in
  let spindle_out_r_to_l = map_rotors_r_to_l config.rotors spindle_in_r_to_l in
  let spindle_in_l_to_r = map_refl config.refl spindle_out_r_to_l in
  let spindle_out_l_to_r = map_rotors_l_to_r config.rotors spindle_in_l_to_r in
  map_plug config.plugboard @@ index_inv spindle_out_l_to_r

(**
   [bump delta i] is the function [delta] with an additional bump at [i].
*)
let bump (delta : int -> bool) (i : int) : int -> bool =
  fun x -> if x = i then true else delta x

(**
   [rule_2 rotors' h delta] computes a new delta function based on the following
   rule: if the top letter of any rotor except the leftmost is its turnover,
   then that rotor and the rotor to its left step.
   [rotors'] is the reversal of the actual rotors so replace left with right in
   the rule above.
   requires:
   - rotors' is a valid spindle
   - h is in 0..[length rotors' - 1]
*)
let rec rule_2 (rotors' : oriented_rotor list) (h : int) (delta : int -> bool)
  : int -> bool =
  match rotors' with
  | [] -> delta
  | _ :: [] -> delta
  | r1 :: r2 :: tl ->
    begin
      rule_2 (r2 :: tl) (h + 1) (
        if r1.top_letter = r1.rotor.turnover then
          bump (bump delta h) (h + 1)
        else
          delta
      )
    end

(**
   [rotate rotors' h delta] rotates [rotors'] based on [delta].
   [h] is the index of the current head.
   requires:
   - rotors' is a valid spindle
   - h is in 0..[length rotors' - 1]
*)
let rec rotate (rotors' : oriented_rotor list) (h : int) (delta : int -> bool)
  : oriented_rotor list =
  match rotors' with
  | [] -> []
  | r :: tl ->
    begin
      let r' = 
        if delta h then
          let next_letter = next_char r.top_letter in
          { rotor = r.rotor ; top_letter = next_letter }
        else
          r
      in
      r' :: rotate tl (h + 1) delta
    end

(**
   [step config] is the new configuration to which the Enigma machine
   transitions when it steps beginning in configuration [config].
   requires: [config] is a valid configuration
*)
let step (config : config) : config =
  let { refl ; rotors ; plugboard } = config in
  let rotors' = List.rev rotors in
  let delta = rule_2 rotors' 0 (fun x -> x = 0) in
  let new_rotors' = rotate rotors' 0 delta in
  { refl ; rotors = List.rev new_rotors' ; plugboard }

(**
   [step_n config n] applies [step] [n] times
   requires: [n] >= 0
*)
let rec step_n (config : config) (n : int) : config =
  assert (n >= 0);
  if n = 0 then
    config
  else
    step_n (step config) (n - 1)

(**
   [cipher config s] is the string to which [s] enciphers
   when the Enigma machine begins in configuration [config].
   requires:
   - [config] is a valid configuration
   - [s] contains only uppercase letters
*)
let cipher (config : config) (s : string) : string =
  let step_at i = step_n config (i + 1) in
  let f i c = cipher_char (step_at i) c in
  String.mapi f s

