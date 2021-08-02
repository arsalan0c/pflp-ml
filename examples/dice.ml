open Base
open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf

let die =
  uniform
    [
      Atom (Int 1);
      Atom (Int 2);
      Atom (Int 3);
      Atom (Int 4);
      Atom (Int 5);
      Atom (Int 6);
    ]

let rec roll_dice n =
  if n = 0 then certainly (Atom (Lst []))
  else if n > 0 then join_with concat die (roll_dice (n - 1))
  else failwith ">= 0 dice rolls required"

let all_six n =
  (fun dice ->
    match dice with
    | Atom (Lst l) ->
        List.fold l ~init:true ~f:(fun acc c ->
            match c with Int i -> acc && i = 6 | _ -> false)
    | _ -> failwith "expected list of dice")
  $$ roll_dice n

let all_five_or_six n =
  (fun dice ->
    match dice with
    | Atom (Lst l) ->
        List.fold l ~init:true ~f:(fun acc c ->
            match c with Int i -> acc && (i = 6 || i = 5) | _ -> false)
    | _ -> failwith "expected list of dice")
  $$ roll_dice n

let%expect_test _ =
  printf "roll a die\n%s" (stream_print (roll_dice 1 empty_state));
  [%expect
    {| 
    roll a die
    [([1], 0.16666666666666666), ]
    [([2], 0.16666666666666666), ]
    [([3], 0.16666666666666666), ]
    [([4], 0.16666666666666666), ]
    [([5], 0.16666666666666666), ]
    [([6], 0.16666666666666666), ] |}];
  printf "six dice, all show six\n%f\n\n" (all_six 2 empty_state);
  [%expect {| 
    six dice, all show six
    0.027778 |}];
  printf "three dice, all show either five or six\n%f\n\n"
    (all_five_or_six 3 empty_state);
  [%expect {| 
    three dice, all show either five or six
    0.037037 |}]
