open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib.Input

let read_int64_num x =
  let rec aux acc cnt =
    match cnt with
    | 0 -> acc
    | x -> aux (read_int64 () :: acc) Int.(x - 1)
  in
  aux [] x
;;

let get_upper_bound current factor =
  let open Int64 in
  let numer = 100L * current in
  let denom = 100L - factor in
  if numer % denom = 0L then numer / denom else (numer / denom) + 1L
;;

let rec build_list init factor =
  let open Int64 in
  let limit = pow 10L 16L in
  if init > limit then [] else init :: build_list (get_upper_bound init factor) factor
;;

let calc other list =
  let open Int64 in
  let rec aux list cnt =
    match list with
    | [] -> cnt
    | hd :: tl -> if other < hd then cnt else aux tl (cnt + 1L)
  in
  aux list 0L
;;

let () =
  match read_int64_list () with
  | [ n; _; k; fac ] ->
    let competitors = read_int64_num @@ Int.of_int64_exn n in
    let myself = read_int64 () in
    let needed_times =
      if Int64.(fac = 100L)
      then Int64.of_int @@ List.count ~f:(fun x -> Int64.(x >= myself)) competitors
      else (
        let bounds = build_list myself fac in
        List.fold ~init:0L ~f:(fun acc x -> Int64.(acc + calc x bounds)) competitors)
    in
    if Int64.(needed_times > k) then print_endline "NO" else print_endline "YES"
  | _ -> assert false
;;
