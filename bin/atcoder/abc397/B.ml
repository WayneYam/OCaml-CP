open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let calc (s : char list) =
  let s = [ 'o' ] @ s @ [ 'i' ] in
  let res, _ =
    List.fold s ~init:(0, None) ~f:(fun (count, last_char) cur_char ->
      let nxt_count =
        match last_char with
        | None -> count
        | Some x -> if Char.( = ) cur_char x then count + 1 else count
      in
      nxt_count, Some cur_char)
  in
  res
;;

let () =
  let s = read_line () in
  printf "%d" @@ calc @@ String.to_list s
;;

(** End of file *)
