open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let _ = read_int () in
  let lst = read_int_list () in
  let rec test h1 h2 l =
    match l with
    | [] -> false
    | h3 :: l -> if h1 = h2 && h2 = h3 then true else test h2 h3 l
  in
  let ans =
    match lst with
    | h1 :: h2 :: l -> test h1 h2 l
    | _ -> false
  in
  printf "%s" (if ans then "Yes\n" else "No\n")
;;

(** End of file *)
