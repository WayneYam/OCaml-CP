open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let _, m = read_int_list () |> to_2ple in
  let edges =
    List.init m ~f:(fun _ ->
      let x, y = read_int_list () |> to_2ple in
      if x < y then x, y else y, x)
  in
  edges
  |> List.sort_and_group ~compare:[%compare: int * int]
  |> List.sum
       (module Int)
       ~f:(fun l ->
         let (x, y), cnt = List.hd_exn l, List.length l in
         if x = y then cnt else cnt - 1)
  |> printf "%d\n"
;;

(** End of file *)
