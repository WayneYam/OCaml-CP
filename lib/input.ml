open! Base
open! Core
open! Stdio

let read_line () = In_channel.(input_line_exn stdin)
let read_int () = Int.of_string @@ read_line ()

let read_int_list ?(on = [ ' ' ]) () =
  List.map ~f:Int.of_string @@ String.split_on_chars ~on @@ read_line ()
;;
