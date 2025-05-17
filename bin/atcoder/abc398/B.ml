open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let lst =
    read_int_list ()
    |> List.sort_and_group ~compare
    |> List.map ~f:List.length
    |> List.sort ~compare:descending
  in
  let ans =
    match lst with
    | hd1 :: hd2 :: _ when hd1 >= 3 && hd2 >= 2 -> "Yes\n"
    | _ -> "No\n"
  in
  printf "%s" ans
;;

(** End of file *)
