open! Base
open! Core
open! Stdio

(** Definition of modules here *)

open! Lib.Input

module T = struct
  type t = int * int [@@deriving compare, sexp]
end

module S = Set.Make (T)

let () = print_endline "Hello World!"

(** End of file *)
