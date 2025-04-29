open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n = read_int () in
  let ans = if 400 % n > 0 then -1 else 400 / n in
  printf "%d\n" ans

(** End of file *)
