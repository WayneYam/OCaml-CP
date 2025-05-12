open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let _ = read_int () in
  let s, t = read_line (), read_line () in
  let s, t = String.to_list s, String.to_list t in
  List.map2_exn s t ~f:(fun c1 c2 -> not @@ Char.equal c1 c2)
  |> List.count ~f:Fn.id
  |> printf "%d\n"
;;

(** End of file *)
