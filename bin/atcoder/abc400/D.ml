open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

module Dat = struct
  module T = struct
    type t = int * int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module PQ = Set.Make (Dat)

let () =
  let h, w = read_int_list () |> to_2ple in
  let board = Array.init h ~f:(fun _ -> read_line ()) in
  let pq = () in
  ()
;;

(** End of file *)
