open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n, m = read_int_list () |> to_2ple in
  let edges = List.init m ~f:(fun _ -> read_int_list () |> to_2ple) in
  let dsu = Dsu.create (n + 1) in
  List.iter edges ~f:(fun (x, y) -> Dsu.merge dsu x y |> ignore);
  let cc_count =
    List.init n ~f:(fun x -> x + 1)
    |> List.map ~f:(Dsu.leader dsu)
    |> List.counti ~f:(fun id head -> id + 1 = head)
  in
  printf "%d\n" @@ (m - n + cc_count)
;;

(** End of file *)
