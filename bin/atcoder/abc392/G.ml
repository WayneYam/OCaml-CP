open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let maxN = Int.pow 2 21

module M = (val Ntt.make 998244353 3 maxN : Ntt.NTT)

let maxC = 1000001

let () =
  let _ = read_int () in
  let a = read_int_list () in
  let poly = Array.init (maxC + 1) ~f:(fun _ -> 0) in
  List.iter ~f:(fun x -> poly.(x) <- 1) a;
  let poly2 = M.(poly * poly) in
  let calc () =
    let rec aux acc i =
      if i <= maxC then aux (acc + (poly.(i) * (poly2.(2 * i) - 1) / 2)) (i + 1) else acc
    in
    aux 0 0
  in
  printf "%d\n" @@ calc ()
;;

(** End of file *)
