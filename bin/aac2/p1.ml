open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let _ = read_int () in
  let s = read_line () |> String.to_list in
  let arr = Array.init 26 ~f:(fun _ -> 0) in
  let rec add s =
    match s with
    | [] -> ()
    | hd :: tl ->
      let id = Char.to_int hd - Char.to_int 'a' in
      arr.(id) <- arr.(id) + 1;
      add tl
  in
  add s;
  printf "%d" @@ max (Array.count ~f:(fun x -> x % 2 = 1) arr) 1
;;

(** End of file *)
