open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n = read_int () in
  let qs = List.init n ~f:(fun _ -> read_line ()) |> List.rev in
  let rec aux acc lst state =
    match lst with
    | [] -> acc
    | hd :: tl -> (
        match hd with
        | "public" -> aux acc tl state
        | "private" ->
            let acc' = if state then acc else acc + 1 in
            aux acc' tl state
        | "login" -> aux acc tl true
        | "logout" -> aux acc tl false
        | _ -> assert false)
  in
  printf "%d\n" @@ aux 0 qs false

(** End of file *)
