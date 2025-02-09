open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let find (n1, d1) (n2, d2) =
  let base = n1 * n2 in
  let rec aux acc d1 d2 =
    match d1, d2 with
    | [], _ | _, [] -> acc
    | (h1, c1) :: tl1, (h2, c2) :: tl2 ->
      let c = compare h1 h2 in
      if c = 0
      then aux (acc + (c1 * c2)) tl1 tl2
      else if c = 1
      then aux acc d1 tl2
      else aux acc tl1 d2
  in
  Float.(of_int (aux 0 d1 d2) / of_int base)
;;

let () =
  let n = read_int () in
  let lst =
    List.init n ~f:(fun _ ->
      let lst = read_int_list () in
      ( List.hd_exn lst
      , List.tl_exn lst
        |> List.sort_and_group ~compare
        |> List.map ~f:(fun x -> List.hd_exn x, List.length x) ))
  in
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | hd :: tl ->
      let cur = List.fold ~init:0.0 ~f:(fun acc x -> Float.max acc (find hd x)) tl in
      aux Float.(max acc cur) tl
  in
  printf "%.15f\n" @@ aux 0.0 lst
;;

(** End of file *)
