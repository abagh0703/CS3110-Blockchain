open Random

module RSA = struct
  type key = {m:int; (* m = p*q *)
              p:int;
              q:int;
              phi:int;
              pubk:int; (* unit [k] of phi(m)*)
              privk:int; (*inverse of [k] mod phi(m)*)
             }
             (** The type of RSA keys.  Components [size], [m] and [pubk] define
                 the public part of the key.  Components [size], [m] and [privk]
                 define the private part of the key. *)

  let prime =
    [101;103;107;109;113;127;131;137;139;149;151;157;163;167;173;
179;181;191;193;197;199;211;223;227;229;233;239;241;251;257;263;269;271;277;281;
283;293;307;311;313;317;331;337;347;349;353;359;367;373;379;383;389;397;401;409;
419;421;431;433;439;443;449;457;461;463;467;479;487;491;499;503;509;521;523;541;
547;557;563;569;571;577;587;593;599;601;607;613;617;619;631;641;643;647;653;659;
661;673;677;683;691;701;709;719;727;733;739;743;751;757;761;769;773;787;797;809;
811;821;823;827;829;839;853;857;859;863;877;881;883;887;907;911;919;929;937;941;
     947;953;967;971;977;983;991;997]
(*length is 143, all primes of 3 digits*)

  (* [gcd] returns the greatest common factor of a and b *)
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;

  (* [gcd_ext] returns a triple with the first two entries as the bezout
   * coefficients, and the last entry as the greatest common divisor of
   * the two numbers *)
  let rec gcd_bezout a = function
    | 0 -> (1, 0, a)
    | b ->
        let s, t, g = gcd_bezout b (a mod b) in
        (t, s - (a / b) * t, g)

  (* [mod_inv] returns of modular inverse of [k] mod [phi] *)
  let mod_inv k phi =
    let mk_pos x = if x < 0 then x + phi else x in
    match gcd_bezout k phi with
    | i, _, 1 -> mk_pos i
    | _ -> failwith "mistake calculating modular inverse"

  (* [generate_unit] randomly generate an integer co-prime with phi *)
  let rec generate_unit phi =
    let test = Random.int phi in
    if test = 0 then generate_unit phi
    else if gcd phi test != 1 then generate_unit phi
    else test

  (** Generate a new, random RSA key.  The result of [new_key] is a
    complete RSA key with all components defined *)
  let new_key =
    let indp = Random.int (List.length prime) in
    let indq = Random.int (List.length prime) in
    let p = List.nth prime indp in
    let q = List.nth prime indq in
    let m = p * q in
    let phi = (p - 1) * (q - 1) in
    let pubk = generate_unit phi in
    let privk = mod_inv pubk phi in
    {m = m; p = p; q = q; phi = phi; pubk = pubk; privk = privk}

  (** [encrypt key msg] encrypts the string [msg] with the public part
      of key [key] (msg**pubk).
      [msg] must be smaller than [key.m] as required.*)
  let encrypt key msg =
    if msg > key.m then failwith "Message is too long"
    else let c = (float_of_int msg)**(float_of_int key.pubk) in
      int_of_float c

  (** [decrypt k msg] decrypts the encrypted c with the
      private part of key [key] (msg**[pubk]**[privk]) *)
  let decrypt key c =
    let msg = (float_of_int c) ** (float_of_int key.privk) in
    int_of_float msg
end
