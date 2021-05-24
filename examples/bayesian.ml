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
 
let run () = begin
  printf "it will rain\n%s" (stream_print (rain empty_state));
  printf "grass is wet\n%f\n\n" (grass_wet_true empty_state)
end
