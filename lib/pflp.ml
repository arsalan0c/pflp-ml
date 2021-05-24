open Base
open Mk.Micro
open Mk.Mini

exception PflpFailure of string
let[@inline] failwith msg = raise (PflpFailure msg)

type probability = float

let probability p = Atom (Float p)
let dist x p = call_fresh (fun v -> v === Pair (x, probability p))

let dist_to_probability = function
  | Pair (_, Atom (Float p)) -> p
  | _ -> failwith "distreq"
let dist_to_event = function
  | Pair (x, Atom (Float _)) -> x
  | _ -> failwith "distreq"

let certainly x = dist x 1.0
let ( $ ) a b = disj a b

let member xs = disj_plus xs

(* probabilities should sum to 1.0 *)
let enum xs ps = 
  match (List.map2 ~f:(fun x p -> dist x p) xs ps) with
  | Ok l -> member l
  | Unequal_lengths -> failwith "unequal number of events and probabilities"
  
let uniform xs = 
  let len = List.length xs in
  if len > 0 then let p = 1.0 /. (Float.of_int len) in enum xs (List.init len ~f:(fun _ -> p))
  else failwith "cannot construct uniform distribution without any events"

let ( >>>= ) g f sc =
  let corral = function
    | Pair (_, Atom (Float p)), Pair (y, Atom (Float q)) -> dist y (p *. q)
    | _ -> failwith "expected two distributions"
  in
  let rec loop s = match s with
    | Nil -> mZero
    | Immature i -> Immature (fun () -> loop (i ()))
    | Cons ((subst, _), rest) ->
      let d = subst_to_vals subst in
      let e = List.map d ~f:dist_to_event in
      let fg = conj_plus (List.map e ~f) in
      let fd = List.fold (stream_to_lst (call_empty_state fg)) ~init:[] ~f:(fun acc (subst, _) -> (subst_to_vals subst)@acc) in
      let cd = List.cartesian_product d fd in                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
      let rg = List.map cd ~f:corral in
      let sg = List.map rg ~f:call_empty_state in
      mplus (List.fold sg ~init:mZero ~f:mplus) (loop rest)
  in loop (g sc)

let join_with f d1 d2 = d1 >>>= (fun x -> d2 >>>= (fun y -> certainly (f x y)))

let filter_dist pred g sc =
  let rec loop s = match s with
    | Nil -> mZero
    | Immature i -> Immature (fun () -> loop (i ()))
    | Cons ((subst, _), rest) ->
      let d = subst_to_vals subst in
      let pd = List.fold d ~init:[] ~f:(fun acc d -> 
        let x = dist_to_event d in
        let p = dist_to_probability d in
        if pred x then (dist x p)::acc else acc
      ) in
      let rd = conj_plus pd in
      mplus (call_empty_state rd) (loop rest)
  in
  loop (g sc)

let ( $$ ) pred g sc = 
  let fg = filter_dist pred g in
  let fd = all_values (fg sc) in
  let p = List.map fd ~f:dist_to_probability in
  List.fold p ~init:0.0 ~f:(+.)
