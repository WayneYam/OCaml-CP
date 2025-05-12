open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let _ = read_int () in
  let input = read_int_list () in
  let map =
    List.sort_and_group input ~compare:descending
    |> List.map ~f:(fun lst -> List.hd_exn lst, List.length lst)
    |> List.fold
         ~init:(Map.empty (module Int), 1)
         ~f:(fun (map, rk) (num, cnt) -> Map.add_exn map ~key:num ~data:rk, rk + cnt)
    |> fst
  in
  input |> List.map ~f:(fun x -> Map.find_exn map x) |> List.iter ~f:(printf "%d\n")
;;

(** End of file *)
