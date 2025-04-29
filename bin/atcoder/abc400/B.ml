open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n, m = read_int_list () |> to_2ple in
  let rec solve acc cur expo =
    if expo > m then `Value acc
    else if acc + cur > 1000000000 then `Infinity
    else solve (acc + cur) (cur * n) (expo + 1)
  in
  match solve 0 1 0 with
  | `Infinity -> printf "inf\n"
  | `Value x -> printf "%d\n" x

(** End of file *)
