open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let x = read_float () in
  printf "%s" @@ if Float.(x >= 38.0) then "1" else if Float.(x >= 37.5) then "2" else "3"
;;

(** End of file *)
