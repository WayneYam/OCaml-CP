open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let x, y, z = read_int_list () |> List.sort ~compare |> to_3ple in
  if x * y = z then print_endline "Yes" else print_endline "No"
;;

(** End of file *)
