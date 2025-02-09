open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let n = read_int () in
  printf "%d" (n - 1)
;;

(** End of file *)
