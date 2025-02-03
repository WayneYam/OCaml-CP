open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib.Input

let solve s r =
  let open Int64 in
  let s = of_int s in
  let r = of_int r in
  if s * s * 100L > r * r * 314L then print_endline "SQUARE" else print_endline "CIRCLE"
;;

let () =
  match read_int_list () with
  | [ x; y ] -> solve x y
  | _ -> assert false
;;
