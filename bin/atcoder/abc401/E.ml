open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let solve n edge =
  let adj = Array.init (n + 1) ~f:(fun _ -> []) in
  List.iter edge ~f:(fun (x, y) -> adj.(x) <- y :: adj.(x));
  let ans = Array.init (n + 1) ~f:(fun _ -> -1) in
  let rec bfs fringe closed =
    let cur_node = Set.min_elt fringe in
    match cur_node with
    | None -> ()
    | Some cur_node ->
      let fringe = Set.remove fringe cur_node in
      let fringe =
        Set.union fringe (Set.diff (Set.of_list (module Int) adj.(cur_node)) closed)
      in
      let closed = Set.add closed cur_node in
      if Set.length closed = Set.max_elt_exn closed
      then (
        let id = Set.length closed in
        ans.(id) <- Set.length fringe);
      bfs fringe closed
  in
  bfs (Set.empty (module Int) |> Fn.flip Set.add 1) (Set.empty (module Int));
  ans |> Array.to_list |> List.tl_exn
;;

let () =
  let n, m = read_int_list () |> to_2ple in
  let edge =
    List.init m ~f:(fun _ -> read_int_list () |> to_2ple)
    |> List.concat_map ~f:(fun (x, y) -> [ x, y; y, x ])
    |> List.sort ~compare:[%compare: int * int]
  in
  List.iter ~f:(printf "%d\n") @@ solve n edge
;;

(** End of file *)
