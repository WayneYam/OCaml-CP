open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let s = "sick" in
  let f = "fine" in
  let x, y = read_string_list () |> to_2ple in
  if String.(x = s && y = s) then printf "1\n";
  if String.(x = s && y = f) then printf "2\n";
  if String.(x = f && y = s) then printf "3\n";
  if String.(x = f && y = f) then printf "4\n"
;;

(** End of file *)
