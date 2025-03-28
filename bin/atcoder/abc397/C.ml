open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let get_occurences lst =
  lst
  |> List.mapi ~f:(fun id x -> x, id)
  |> List.sort ~compare:[%compare: int * int]
  |> List.group ~break:(fun (x1, _) (x2, _) -> x1 <> x2)
  |> List.map ~f:(fun lst -> fst @@ List.hd_exn lst, List.map ~f:snd lst)
;;

let calc in_lst out_lst =
  let rec aux mx cur in_lst out_lst =
    match in_lst, out_lst with
    | [], _ -> mx
    | _, [] -> assert false
    | h_i :: t_i, h_o :: t_o ->
      if h_i = h_o
      then aux mx cur t_i t_o
      else if h_i < h_o
      then (
        let cur' = cur + 1 in
        aux (max mx cur') cur' t_i out_lst)
      else (
        let cur' = cur - 1 in
        aux mx cur' in_lst t_o)
  in
  aux 0 0 in_lst out_lst
;;

let solve lst =
  let occurences = get_occurences lst in
  let distinct = List.length @@ List.sort_and_group ~compare lst in
  let first_occurences =
    List.map occurences ~f:(fun (_, l) -> List.hd_exn l) |> List.sort ~compare
  in
  let last_occurences =
    List.map occurences ~f:(fun (_, l) -> List.last_exn l) |> List.sort ~compare
  in
  distinct + calc first_occurences last_occurences
;;

let () =
  let _ = read_int () in
  let lst = read_int_list () in
  printf "%d" @@ solve lst
;;

(** End of file *)
