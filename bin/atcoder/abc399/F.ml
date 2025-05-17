open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input
include Modint_prime.O1

let make_c n =
  let fac = Array.init (n + 1) ~f:(fun _ -> 1) in
  for i = 1 to n do
    fac.(i) <- fac.(i - 1) *% i
  done;
  fun a b -> if b < 0 || b > a then 0 else fac.(a) /% fac.(b) /% fac.(a - b)
;;

let () =
  let _, k = read_int_list () |> to_2ple in
  let a = read_int_list () in
  let c =
    let calc_c = make_c k k in
    Array.init (k + 1) ~f:calc_c
  in
  let rec calc acc prefix lst sum_pow =
    match lst with
    | [] -> acc
    | hd :: lst ->
      let prefix = prefix +% hd in
      let acc =
        Array.foldi sum_pow ~init:acc ~f:(fun i acc s ->
          let mul1 = prefix ^% (k - i) in
          let mul2 = c.(i) in
          let sign = if i land 1 > 0 then m - 1 else 1 in
          (mul1 *% mul2 *% sign *% s) +% acc)
      in
      Array.iteri sum_pow ~f:(fun i v -> sum_pow.(i) <- v +% (prefix ^% i));
      calc acc prefix lst sum_pow
  in
  let sum_pow = Array.init (k + 1) ~f:(fun id -> if id = 0 then 1 else 0) in
  let ans = calc 0 0 a sum_pow in
  printf "%d\n" ans
;;

(** End of file *)
