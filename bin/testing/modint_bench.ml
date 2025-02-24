open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let test_int64 m =
  let m = Int64.of_int m in
  let sz = 100000000 in
  let rec calc acc i =
    if i > sz then acc else calc Int64.(acc * Int64.of_int i % m) (i + 1)
  in
  printf "%d\n" @@ Int.of_int64_exn (calc 1L 1)
;;

let test_int m =
  let sz = 100000000 in
  let rec calc acc i = if i > sz then acc else calc (acc * i mod m) (i + 1) in
  printf "%d\n" @@ calc 1 1
;;

let test_modint m =
  let module M = (val Modint.make m : Modint.Modint) in
  let sz = 100000000 in
  let rec calc acc i =
    let i' = M.of_int i in
    if i > sz then acc else calc M.(acc * i') (i + 1)
  in
  printf "%d\n" @@ M.to_int @@ calc (M.of_int 1) 1
;;

let () =
  let m = read_int () in
  test_modint m
;;

(** End of file *)
