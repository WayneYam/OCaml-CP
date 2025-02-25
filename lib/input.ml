open! Base
open! Core
open! Stdio

let read_line () = In_channel.(input_line_exn stdin)
let read_int () = Int.of_string @@ read_line ()
let read_string_list ?(on = [ ' ' ]) () = String.split_on_chars ~on @@ read_line ()

let read_int_list ?(on = [ ' ' ]) () =
  List.map ~f:Int.of_string @@ read_string_list ~on ()
;;

let read_int64 () = Int64.of_string @@ read_line ()

let read_int64_list ?(on = [ ' ' ]) () =
  List.map ~f:Int64.of_string @@ String.split_on_chars ~on @@ read_line ()
;;

let to_2ple l =
  match l with
  | [ x; y ] -> x, y
  | _ -> assert false
;;

let to_3ple l =
  match l with
  | [ x; y; z ] -> x, y, z
  | _ -> assert false
;;

let to_4ple l =
  match l with
  | [ x; y; z; w ] -> x, y, z, w
  | _ -> assert false
;;
