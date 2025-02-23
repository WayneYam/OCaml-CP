open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let maxN = Int.pow 2 22

module F = (val Fft.make maxN : Fft.FFT)

let maxC = 1000000

let () =
  let _ = read_int () in
  let a = read_int_list () in
  let poly = Array.init (maxC + 1) ~f:(fun _ -> 0) in
  List.iter ~f:(fun x -> poly.(x) <- 1) a;
  let poly' = Array.map poly ~f:(fun x -> ({ re = Float.of_int x; im = 0. } : F.cp)) in
  let poly2 =
    F.(poly' * poly') |> Array.map ~f:(fun { re; _ } -> Float.iround_nearest_exn re)
  in
  let calc () =
    let rec aux acc i =
      if i <= maxC then aux (acc + (poly.(i) * (poly2.(2 * i) - 1) / 2)) (i + 1) else acc
    in
    aux 0 0
  in
  printf "%d\n" @@ calc ()
;;

(** End of file *)
