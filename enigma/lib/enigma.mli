(* [index c] is the 0-based index of [c] in the alphabet
 * requires: [c] is an uppercase character in A..Z *)
val index : char -> int

(* [map_r_to_l wiring top_letter input_pos] is the left-hand output position
 * at which current would appear when current enters at right-hand input
 * position [input_pos] to a rotor whose wiring specification is given by
 * [wiring].  The orientation of the rotor is given by [top_letter],
 * which is the top letter appearing to the operator in the rotor's
 * present orientation.
 * requires:
 *  - [wiring] is a valid wiring specification.
 *  - [top_letter] is in 'A'..'Z'
 *  - [input_pos] is in 0..25
 *)
val map_r_to_l : string -> char -> int -> int

(* [index c] is the 0-based index of [c] in the alphabet
 * requires: [c] is an uppercase character in A..Z *)
val map_l_to_r : string -> char -> int -> int

(* [map_refl wiring input_pos] is the output position at which current would
 * appear when current enters at input position [input_pos] to a reflector
 * whose wiring specification is given by [wiring].
 * requires:
 *  - [wiring] is a valid reflector specification.
 *  - [input_pos] is in 0..25
 *)
val map_refl : string -> int -> int

(* [map_plug plugs c] is the letter to which [c] is transformed
 * by the plugboard [plugs].
 * requires:
 *  - [plugs] is a valid plugboard
 *  - [c] is in 'A'..'Z'
 *)

val map_plug : (char * char) list -> char -> char


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

(* [cipher_char config c] is the letter to which the Enigma machine
 * ciphers input [c] when it is in configuration [config].
 * requires:
 *  - [config] is a valid configuration
 *  - [c] is in 'A'..'Z'
 *)
val cipher_char : config -> char -> char

(* [step config] is the new configuration to which the Enigma machine
 * transitions when it steps beginning in configuration [config].
 * requires: [config] is a valid configuration
 *)
val step : config -> config

(* [cipher config s] is the string to which [s] enciphers
 * when the Enigma machine begins in configuration [config].
 * requires:
 *   - [config] is a valid configuration
 *   - [s] contains only uppercase letters
 *)
val cipher : config -> string -> string
