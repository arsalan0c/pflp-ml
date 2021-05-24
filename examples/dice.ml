open Base

open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf


let die = uniform [Atom (Int 1); Atom (Int 2); Atom (Int 3); Atom (Int 4); Atom (Int 5); Atom (Int 6)]

let rec roll_dice n =
  if n = 0 then certainly (Atom (Lst []))
  else if n > 0 then join_with concat die (roll_dice (n - 1)) 
  else failwith ">= 0 dice rolls required" 

let all_six n = (fun dice -> match dice with 
    | (Atom (Lst l)) -> List.fold l ~init:true ~f:(
        fun acc c -> match c with (Int i) -> acc && (i = 6) | _ -> false
      )
    | _ -> failwith "expected list of dice"
  ) $$ (roll_dice n)

let all_five_or_six n = (fun dice -> match dice with 
    | (Atom (Lst l)) -> List.fold l ~init:true ~f:(
        fun acc c -> match c with (Int i) -> acc && (i = 6 || i = 5) | _ -> false
      )
    | _ -> failwith "expected list of dice"
  ) $$ (roll_dice n)

let run () = begin
  printf "roll a die\n%s" (stream_print (roll_dice 1 empty_state));
  printf "six dice, all show six\n%f\n\n" (all_six 2 empty_state);
  printf "three dice, all show either five or six\n%f\n\n" (all_five_or_six 3 empty_state)
end
