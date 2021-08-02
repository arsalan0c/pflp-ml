open Base
open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf

let flip p = enum [ Atom (Bool true); Atom (Bool false) ] [ p; 1.0 -. p ]

let rain = flip 0.4

(* whether the sprinkler is on depends on whether it is raining *)
let sprinkler rain =
  match rain with
  | Atom (Bool false) -> flip 0.4
  | Atom (Bool true) -> flip 0.01
  | _ -> assert false

(* whether the grass is wet depends on whether the sprinkler is on and whether it is raining *)
let grass_wet sprinkler rain =
  match (sprinkler, rain) with
  | Atom (Bool true), Atom (Bool true) -> flip 0.99
  | Atom (Bool true), Atom (Bool false) -> flip 0.9
  | Atom (Bool false), Atom (Bool true) -> flip 0.8
  | Atom (Bool false), Atom (Bool false) -> flip 0.0
  | _ -> assert false

let grass_wet_true =
  (fun g -> match g with Atom (Bool b) -> b | _ -> assert false)
  $$ ( rain >>>= fun r ->
       sprinkler r >>>= fun s -> grass_wet s r )

let grass_wet_when_rain =
  (fun g -> match g with Atom (Bool b) -> b | _ -> assert false)
  $$
  let r = Atom (Bool true) in
  sprinkler r >>>= fun s -> grass_wet s r

(* P(grass_wet and rain) = P(grass wet | rain)P(rain) *)
let grass_wet_and_rain =
  (fun g -> match g with Atom (Bool b) -> b | _ -> assert false)
  $$ join_with
       (fun g r ->
         match (g, r) with
         | Atom (Bool g), Atom (Bool r) -> Atom (Bool (g && r))
         | _ -> assert false)
       (let r = Atom (Bool true) in
        sprinkler r >>>= fun s -> grass_wet s r)
       rain

(* P(rain | grass wet) = P(grass wet and rain) / P(grass wet) *)
let rain_when_grass_wet =
  grass_wet_and_rain empty_state /. grass_wet_true empty_state

(* alternative approach based on model data type: rain, sprinkler, grass wet *)
type grassmodel = { r_ : bool; s_ : bool; g_ : bool }

let grass_model =
  rain >>>= fun r ->
  sprinkler r >>>= fun s ->
  grass_wet s r >>>= fun g ->
  match (r, s, g) with
  | Atom (Bool b_r), Atom (Bool b_s), Atom (Bool b_g) ->
      certainly (Atom (Lst [ Bool b_r; Bool b_s; Bool b_g ]))
  | _ -> assert false

let grass_wet_true_ =
  (fun model ->
    match model with Atom (Lst [ _; _; Bool b_g ]) -> b_g | _ -> assert false)
  $$ grass_model

let grass_wet_and_rain_ =
  (fun model ->
    match model with
    | Atom (Lst [ Bool b_r; _; Bool b_g ]) -> b_r && b_g
    | _ -> assert false)
  $$ grass_model

(* conditional probabilities *)
let cond_prob ps dx =
  (fun model ->
    match model with
    | Atom (Lst l) -> (
        match
          List.fold2 l ps ~init:true ~f:(fun so_far a b2 ->
              match (b2, a) with
              | true, Bool true -> so_far && true
              | true, Bool false -> false
              | false, _ -> so_far
              | _ -> assert false)
        with
        | Ok b -> b
        | Unequal_lengths -> assert false )
    | _ -> assert false)
  $$ dx

let grass_cond_prob ps = cond_prob ps grass_model

let rain_when_grass_wet_ =
  grass_cond_prob [ true; false; true ] empty_state
  /. grass_cond_prob [ false; false; true ] empty_state

let sprinkler_when_grass_wet =
  grass_cond_prob [ false; true; true ] empty_state
  /. grass_cond_prob [ false; false; true ] empty_state

let%expect_test "grass_model" =
  printf "it will rain\n%s\n" (stream_print (rain empty_state));
  [%expect {| 
    it will rain
    [(true, 0.4), ]
    [(false, 0.6), ] |}];
  printf "grass is wet\n%f\n\n" (grass_wet_true empty_state);
  [%expect {| 
    grass is wet
    0.536760 |}];
  printf "grass wet and rain\n%f\n\n" (grass_wet_and_rain empty_state);
  [%expect {| 
    grass wet and rain
    0.320760 |}];
  printf "grass is wet 2\n%f\n\n" (grass_wet_true_ empty_state);
  [%expect {| 
    grass is wet 2
    0.536760 |}];
  printf "grass wet and rain 2\n%f\n\n" (grass_wet_and_rain_ empty_state);
  [%expect {| 
    grass wet and rain 2 
    0.320760 |}];
  printf "grass wet when rain\n%f\n\n" (grass_wet_when_rain empty_state);
  [%expect {| 
    grass wet when rain 
    0.801900 |}];
  printf "rain when grass wet\n%f\n\n" rain_when_grass_wet;
  [%expect {| 
    rain when grass wet 
    0.597586 |}];
  printf "sprinkler when grass wet\n%f\n\n" sprinkler_when_grass_wet;
  [%expect {| 
    sprinkler when grass wet 
    0.409792 |}];
  printf "rain when grass wet 2\n%f\n\n" rain_when_grass_wet_;
  [%expect {| 
    rain when grass wet 2 
    0.597586 |}]
