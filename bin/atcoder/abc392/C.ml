open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let n = read_int () in
  let p = 0 :: read_int_list () |> Array.of_list in
  let q = 0 :: read_int_list () |> Array.of_list in
  let q_inv = Array.init (n + 1) ~f:(fun x -> x) in
  for i = 1 to n do
    q_inv.(q.(i)) <- i
  done;
  for i = 1 to n do
    printf "%d " q.(p.(q_inv.(i)))
  done;
  print_endline ""
;;

(** End of file *)
