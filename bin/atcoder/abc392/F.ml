open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let n = read_int () in
  let a = read_int_list () in
  let (answer : int option array) = Array.init n ~f:(fun _ -> None) in
  let fen = Fenwicktree.create n in
  for i = 0 to n - 1 do
    Fenwicktree.set fen ~pos:i ~v:1
  done;
  let rec process a id =
    match a with
    | [] -> ()
    | hd :: tl ->
      process tl (id + 1);
      let pos =
        (Option.value_exn @@ Fenwicktree.binary_search fen ~compare `First_equal_to hd)
        - 1
      in
      answer.(pos) <- Some id;
      Fenwicktree.set fen ~pos ~v:0
  in
  process a 1;
  Array.iter answer ~f:(fun x -> x |> Option.value_exn |> printf "%d ")
;;

(** End of file *)
