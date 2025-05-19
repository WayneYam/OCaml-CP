open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

type query =
  | Push of int
  | Pop

let () =
  let st = Stack.create () in
  for _ = 1 to 100 do
    Stack.push st 0
  done;
  let q = read_int () in
  let qs =
    List.init q ~f:(fun _ ->
      match read_int_list () with
      | [ 1; x ] -> Push x
      | [ 2 ] -> Pop
      | _ -> assert false)
    |> List.rev
  in
  List.iter qs ~f:(fun q ->
    match q with
    | Push x -> Stack.push st x
    | Pop -> printf "%d\n" @@ Stack.pop_exn st)
;;

(** End of file *)
