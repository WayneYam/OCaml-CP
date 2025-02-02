open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib.Input

let solve n x =
  let rec get_alt acc count =
    match count with
    | 0 -> acc
    | x -> get_alt (0 :: 1 :: acc) (x - 1)
  in
  let rec add_zero acc count =
    match count with
    | 0 -> acc
    | x -> add_zero (0 :: acc) (x - 1)
  in
  add_zero (get_alt [] ((n - x) / 2)) x
;;

let () =
  match read_int_list () with
  | [ n; x ] ->
    if (n - x) % 2 = 1
    then print_endline "-1"
    else
      List.iteri ~f:(fun idx v -> if idx = n - 1 then printf "%d\n" v else printf "%d " v)
      @@ solve n x
  | _ -> assert false
;;
