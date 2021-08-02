open Base
open Pflp
open Mk.Print
open Mk.Micro

let printf = Stdlib.Printf.printf

let pick_char = uniform [ Atom (Str "a"); Atom (Str "b") ]

let rec random_string n =
  if n = 0 then certainly (Atom (Str ""))
  else if n > 0 then join_with concat pick_char (random_string (n - 1))
  else failwith ">= 0 length of string required"

let palindrome str =
  match str with
  | Atom (Str s) -> String.equal s (String.rev s)
  | _ -> failwith "palindrome: a string atom is required as argument"

let random_is_palindrome n = palindrome $$ random_string n

let explode s =
  let rec loop i l =
    if i < 0 then assert false
    else if i >= String.length s then l
    else
      let c = s.[i] in
      loop (i + 1) l @ [ c ]
  in
  loop 0 []

let consecutive_b str =
  match str with
  | Atom (Str s) ->
      let rec loop l =
        match l with
        | [] -> false
        | 'b' :: 'b' :: _ -> true
        | _ :: rest -> loop rest
      in
      loop (explode s)
  | _ -> failwith "consecutive_b: a string atom is required as argument"

let random_has_consec_b n = consecutive_b $$ random_string n

let run () =
  printf "random string of length 3\n%s"
    (stream_print (random_string 3 empty_state));
  printf "random string of length 5 is a palindrome\n%f\n\n"
    (random_is_palindrome 5 empty_state);
  printf "random string of length 10 has two consecutive b's\n%f\n\n"
    (random_has_consec_b 10 empty_state)
