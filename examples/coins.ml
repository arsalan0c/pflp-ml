open Base
open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf

let coin = dist (Atom (Bool true)) 0.5 $ dist (Atom (Bool false)) 0.5

let uniform_coin = uniform [ Atom (Bool true); Atom (Bool false) ]

let independent_coins =
  coin >>>= fun b1 ->
  coin >>>= fun b2 -> certainly (Pair (b1, b2))

let dependent_coins =
  coin >>>= fun b ->
  match b with Atom (Bool true) -> coin | _ -> certainly (Atom (Bool false))

let rec flip_coin n =
  if n = 0 then certainly (Atom (Lst []))
  else if n > 0 then join_with concat coin (flip_coin (n - 1))
  else failwith ">= 0 coin tosses required"

let gte_two_heads_in_four =
  (fun coins ->
    match coins with
    | Atom (Lst l) ->
        let heads =
          List.fold l ~init:0 ~f:(fun acc c ->
              match c with Bool b when b -> acc + 1 | _ -> acc)
        in
        heads >= 2
    | _ -> failwith "expected list of coins")
  $$ flip_coin 4

let%expect_test _ =
  printf "coin\n%s\n" (stream_print (coin empty_state));
  [%expect {| 
    coin
    [(true, 0.5), ]
    [(false, 0.5), ] |}];
  printf "uniform coin\n%s\n" (stream_print (uniform_coin empty_state));
  [%expect {| 
    uniform coin
    [(true, 0.5), ]
    [(false, 0.5), ] |}];
  printf "independent coins\n%s\n"
    (stream_print (independent_coins empty_state));
  [%expect
    {| 
    independent coins
    [((true, false), 0.25), ]
    [((true, true), 0.25), ]
    [((false, false), 0.25), ]
    [((false, true), 0.25), ] |}];
  printf "dependent coins\n%s\n" (stream_print (dependent_coins empty_state));
  [%expect
    {| 
    dependent coins
    [(false, 0.25), ]
    [(true, 0.25), ]
    [(false, 0.5), ] |}];
  printf "flip 2 coins\n%s\n" (stream_print (flip_coin 2 empty_state));
  [%expect
    {| 
    flip 2 coins
    [([true, false], 0.25), ]
    [([true, true], 0.25), ]
    [([false, false], 0.25), ]
    [([false, true], 0.25), ] |}];
  printf "probability of >= 2 heads in four flips\n%f\n\n"
    (gte_two_heads_in_four empty_state);
  [%expect {| 
    probability of >= 2 heads in four flips
    0.687500 |}]
