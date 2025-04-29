open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

(*
   let int_square_root k =
  if k < 0
  then None
  else
    Binary_search.binary_search
      ~get:(fun () id -> id)
      ~length:(fun () -> min (k+1) 2000000000)
      ()
      ~compare:(fun x target -> compare (x * x) target)
      `First_equal_to
      k
;;
*)

let solve_quad a b c =
  let a' = Float.of_int a in
  let b' = Float.of_int b in
  let c' = Float.of_int c in
  let d' = Float.((b' * b') - (4.0 * a' * c')) in
  let sd' = Float.sqrt d' in
  let test_values =
    Float.[ (-b' + sd') / (2.0 * a'); (-b' - sd') / (2.0 * a') ]
  in
  List.filter_map test_values ~f:(fun x ->
      if Float.(x <= zero) then None
      else
        let x' = round ~dir:`Nearest x in
        if Float.(abs (x' - x) > 1e-6) then None else Some (Float.to_int x'))
  |> List.hd

let solve n =
  let rec test t =
    if t * t * t > n then None
    else if n % t <> 0 then test (t + 1)
    else
      match solve_quad 3 (3 * t) ((t * t) - (n / t)) with
      | None -> test (t + 1)
      | Some x -> Some (x + t, x)
  in
  test 1

let () =
  let n = read_int () in
  match solve n with
  | None -> printf "-1\n"
  | Some (x, y) -> printf "%d %d\n" x y

(** End of file *)
