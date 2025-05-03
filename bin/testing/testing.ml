open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input
module Int_hashtbl = Hashtbl.Make (Int)

let () =
  let tbl = Int_hashtbl.create in
  ()
;;

(** End of file *)
