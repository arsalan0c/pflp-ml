open Base

open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf


let coin = (dist (Atom (Bool true)) 0.5) $ (dist (Atom (Bool false)) 0.5)

let uniform_coin = uniform [Atom (Bool true); Atom (Bool false)]

let independent_coins = coin >>>= (fun b1 -> coin >>>= fun b2 -> certainly (Pair (b1, b2)))

let dependent_coins = coin >>>= (fun b ->
  match b with
    | Atom (Bool true) -> coin
    | _ -> certainly (Atom (Bool false))
)

let rec flip_coin n = 
  if n = 0 then certainly (Atom (Lst []))
  else if n > 0 then join_with concat coin (flip_coin (n - 1)) 
  else failwith ">= 0 coin tosses required"
  
let gte_two_heads_in_four = (fun coins -> match coins with 
    | (Atom (Lst l)) -> let heads = List.fold l ~init:0 ~f:(
        fun acc c -> match c with (Bool b) when b -> acc + 1 | _ -> acc
      ) in heads >= 2 
    | _ -> failwith "expected list of coins"
  ) $$ (flip_coin 4)

let run () = begin
  printf "coin\n%s" (stream_print (coin empty_state));
  printf "uniform coin\n%s" (stream_print (uniform_coin empty_state));
  printf "independent coins\n%s" (stream_print (independent_coins empty_state));
  printf "dependent coins\n%s" (stream_print (dependent_coins empty_state));
  printf "flip 2 coins\n%s" (stream_print (flip_coin 2 empty_state));
  printf "probability of >= 2 heads in four flips\n%f\n\n" (gte_two_heads_in_four empty_state)
end
