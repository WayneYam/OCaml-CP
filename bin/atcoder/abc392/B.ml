open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input
module M = Set.Make (Int)

let () =
  let n, m = read_int_list () |> to_2ple in
  printf "%d\n" (n - m);
  let big = List.init n ~f:(fun x -> x + 1) |> M.of_list in
  let small = read_int_list () |> List.sort ~compare |> M.of_list in
  Set.diff big small |> Set.to_list |> List.iter ~f:(printf "%d ");
  printf "\n"
;;

(** End of file *)
