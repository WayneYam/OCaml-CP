open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let s = read_line () |> String.to_list |> List.mapi ~f:(fun id c -> id, c) in
  let s' = List.cartesian_product (List.cartesian_product s s) s in
  printf "%d"
  @@ List.count s' ~f:(fun (((x1, c1), (x2, c2)), (x3, c3)) ->
    x1 < x2 && x2 < x3 && x3 - x2 = x2 - x1 && Char.(c1 = 'A' && c2 = 'B' && c3 = 'C'))
;;

(** End of file *)
