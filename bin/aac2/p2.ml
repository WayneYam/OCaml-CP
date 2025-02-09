open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let test_2 arr =
  if
    List.count ~f:(fun x -> x % 2 = 1) arr > 1
    || List.count ~f:(fun x -> x % 2 = 0) arr > 1
  then Some 2
  else None
;;

let test_plus_2 arr x =
  let split lst x =
    let rec aux acc lst =
      match lst with
      | [] -> acc, []
      | hd :: tl -> if hd < x then aux (hd :: acc) tl else acc, lst
    in
    aux [] lst
  in
  let lo, tmp = split arr x in
  let mi, hi = split tmp (x + 1) in
  let rec search lo hi target =
    match lo, hi with
    | [], _ | _, [] -> false
    | hl :: tl, hh :: th ->
      let c = compare (hl + hh) target in
      if c = 0 then true else if c = 1 then search tl hi target else search lo th target
  in
  let len = List.length mi in
  if search lo hi (x * 2) then len + 2 else len
;;

let test_plus_1 arr x =
  let upd (h1, h2) h =
    if h = x then h1, h2 else if h <= h2 then h1, h2 else if h <= h1 then h1, h else h, h1
  in
  let h1, h2 = List.fold ~init:(0, 0) ~f:upd arr in
  let len = List.count arr ~f:(equal x) in
  if h1 + h2 >= x && h2 > 0 then len + 1 else len
;;

let ch_max x y = max x (Option.value y ~default:0)

let () =
  let _ = read_int () in
  let arr = read_int_list () |> List.sort ~compare in
  let ans = 1 in
  let ans = ch_max ans (test_2 arr) in
  let ans = List.fold ~init:ans ~f:(fun acc x -> max acc (test_plus_2 arr x)) arr in
  let ans = List.fold ~init:ans ~f:(fun acc x -> max acc (test_plus_1 arr x)) arr in
  printf "%d\n" ans
;;

(** End of file *)
