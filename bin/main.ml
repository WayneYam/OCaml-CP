open! Base
open! Core
open! Stdio

let () = In_channel.iter_lines Stdio.stdin ~f:print_endline
