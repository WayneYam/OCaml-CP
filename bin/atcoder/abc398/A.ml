open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n = read_int () in
  if n land 1 > 0
  then (
    Fn.apply_n_times ~n:(n / 2) (fun () -> printf "-") ();
    printf "=";
    Fn.apply_n_times ~n:(n / 2) (fun () -> printf "-") ();
    printf "\n")
  else (
    Fn.apply_n_times ~n:((n / 2) - 1) (fun () -> printf "-") ();
    printf "==";
    Fn.apply_n_times ~n:((n / 2) - 1) (fun () -> printf "-") ();
    printf "\n")
;;

(** End of file *)
