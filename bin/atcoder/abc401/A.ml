open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n = read_int () in
  if n < 200 || n >= 300 then print_endline "Failure"
  else print_endline "Success"

(** End of file *)
