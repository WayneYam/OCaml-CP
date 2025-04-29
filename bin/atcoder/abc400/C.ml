open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let int_square_root k =
  if k < 0 then None
  else
    Binary_search.binary_search
      ~get:(fun () id -> id)
      ~length:(fun () -> min (k + 1) 2000000000)
      ()
      ~compare:(fun x target -> compare (x * x) target)
      `Last_less_than_or_equal_to k

let aux x =
  let k = int_square_root x |> Option.value_exn in
  (k + 1) / 2

let () =
  let n = read_int () in
  let rec solve acc n = if n = 0 then acc else solve (acc + aux n) (n / 2) in
  printf "%d" @@ solve 0 (n / 2)

(** End of file *)
