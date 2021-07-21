open Base

open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf

let flip p = enum [Atom (Bool true); Atom (Bool false)] [p; 1.0 -. p]

let rain = flip 0.4

(* whether the sprinkler is on depends on whether it is raining *)
let sprinkler r = match r with
  | Atom (Bool false) -> flip 0.4
  | Atom (Bool true) -> flip 0.01
  | _ -> assert false

(* whether the grass is wet depends on whether the sprinkler is on and whether it is raining *)
let grass_wet s r = match s, r with
  | Atom (Bool true), Atom (Bool true) -> flip 0.99
  | Atom (Bool true), Atom (Bool false) -> flip 0.9
  | Atom (Bool false), Atom (Bool true) -> flip 0.8
  | Atom (Bool false), Atom (Bool false) -> flip 0.0
  | _ -> assert false
  
let grass_wet_true = 
  (fun a -> match a with Atom (Bool b) -> b | _ -> assert false) $$
  (rain >>>= fun r -> sprinkler r >>>= fun s -> grass_wet s r)


(* alternative approach based on model data type *)
type grassmodel = {r_: bool; s_: bool; g_: bool}

let grass_model = rain >>>= fun r -> sprinkler r >>>= fun s -> grass_wet s r >>>= fun g -> 
  match r, s, g with
  | Atom (Bool b_r), Atom (Bool b_s), Atom (Bool b_g) -> certainly (Atom (Lst [Bool b_r; Bool b_s; Bool b_g]))
  | _ -> assert false

let grass_wet_true_ = (fun model -> match model with
  | Atom (Lst [_; _; (Bool b_g)]) -> b_g
  | _ -> assert false
) $$ grass_model

let grass_wet_when_rain = (fun model -> match model with
  | Atom (Lst [Bool b_r; _; Bool b_g]) -> b_r && b_g
  | _ -> assert false
) $$ grass_model

(* conditional probabilities *)
(* let cond_prob ps dx = (fun x -> all) $$ dx *)
(* let grass_cond_prob ps =  *)
(* let rain_grass_wet = ((fun a ) $$ ()) /. 
  (() $$ ()) *)
  
let%expect_test _ = begin
  printf "it will rain\n%s\n" (stream_print (rain empty_state));
  [%expect {| 
    it will rain
    [(true, 0.4), ]
    [(false, 0.6), ] |}];
  printf "grass is wet\n%f\n\n" (grass_wet_true empty_state);
  [%expect {| 
    grass is wet
    0.536760 |}];
  printf "grass is wet 2\n%f\n\n" (grass_wet_true_ empty_state);
  [%expect {| 
    grass is wet 2
    0.536760 |}];
  printf "grass wet when rain\n%f\n\n" (grass_wet_when_rain empty_state);
  [%expect {| 
    grass wet when rain
    0.320760 |}];
end
