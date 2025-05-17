open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let _ = read_int () in
  let result =
    read_int_list ()
    |> List.mapi ~f:(fun i j -> j, i + 1)
    |> List.sort_and_group ~compare:(fun x y -> compare (fst x) (fst y))
    |> List.filter_map ~f:(fun lst ->
      if List.length lst > 1 then None else Some (List.hd_exn lst))
    |> List.last
    |> Option.map ~f:snd
    |> Option.value ~default:(-1)
  in
  printf "%d\n" (result + 1)
;;

(** End of file *)
