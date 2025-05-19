open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let _ = read_int_list () |> to_2ple in
  let black = read_int_list () |> List.sort ~compare:descending in
  let white = read_int_list () |> List.sort ~compare:descending in
  let good_black =
    List.filter black ~f:(fun x -> x > 0) |> List.sum (module Int) ~f:Fn.id
  in
  let best_black =
    let res = List.folding_map black ~init:0 ~f:(fun p c -> p + c, p + c) in
    List.map2_exn res black ~f:(fun x y -> if y < 0 then x else good_black)
  in
  let white_pref = List.folding_map white ~init:0 ~f:(fun p c -> p + c, p + c) in
  let rec solve acc w b =
    match w, b with
    | hw :: tw, hb :: tb -> solve (max acc (hw + hb)) tw tb
    | _ -> acc
  in
  let ans = solve good_black white_pref best_black in
  printf "%d\n" ans
;;

(** End of file *)
