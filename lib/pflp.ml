open Base
open Mk.Micro
open Mk.Mini

exception PflpFailure of string
let[@inline] failwith msg = raise (PflpFailure msg)

type probability = float

let prob p = Atom (Float p)

(** given an event and its probability, constructs a distribution *)
let dist x p = call_fresh (fun v -> v === Pair (x, prob p))

let dist_to_probability = function
  | Pair (_, Atom (Float p)) -> p
  | _ -> failwith "distreq"
let dist_to_event = function
  | Pair (x, Atom (Float _)) -> x
  | _ -> failwith "distreq"

(** creates a single-event distribution with a probability of 1.0 *)
let certainly x = dist x 1.0

(** non-deterministically chooses between the arguments *)
let ( $ ) a b = disj a b

let member xs = disj_plus xs

(** creates a distribution based on a list of events and another list containing the corresponding probabilities
    the probabilities must sum to 1.0 and not be negative *)
let enum xs ps = 
  let _ = begin List.fold ps 
    ~f:(fun so_far p -> if (Float.compare p 0.0 < 0) then failwith "probabilities must be strictly positive" else so_far +. p) 
    ~init:0.0 
  end in
  match (List.map2 ~f:(fun x p -> dist x p) xs ps) with
  | Ok l -> member l
  | Unequal_lengths -> failwith "unequal number of events and probabilities"
  
(** constructs a uniform distribution. there must be at least one event *)
let uniform xs = 
  match List.length xs with
  | len when len > 0 -> let p = 1.0 /. (Float.of_int len) in enum xs (List.init len ~f:(fun _ -> p))
  | _ -> failwith "cannot construct uniform distribution without any events"

(** combines two dependent distributions *)
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

(** combines two independent distributions with respect to a given function *)
let join_with f d1 d2 = d1 >>>= (fun x -> d2 >>>= (fun y -> certainly (f x y)))

(** combines two independent distributions with respect to a given function *)
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

(** queries a distribution for the probability of events that satisfy a given predicate *)
let ( $$ ) pred g sc = 
  let fg = filter_dist pred g in
  let fd = all_values (fg sc) in
  let p = List.map fd ~f:dist_to_probability in
  List.fold p ~init:0.0 ~f:(+.)

