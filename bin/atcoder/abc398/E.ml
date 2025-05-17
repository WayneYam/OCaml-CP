open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

module Pair_int = struct
  type t = int * int [@@deriving sexp, compare]
end

module Pair_Set = Set.Make (Pair_int)

let () =
  let n = read_int () in
  let edges = List.init (n - 1) ~f:(fun _ -> read_int_list () |> to_2ple) in
  let tree = Graph.undirected_graph (n + 1) edges in
  let get_parity tree =
    let parity = Array.init (n + 1) ~f:(fun id -> if id = 0 then -1 else 0) in
    let rec dfs c p =
      List.iter tree.(c) ~f:(fun i ->
        match p with
        | Some p when i = p -> ()
        | _ ->
          parity.(i) <- parity.(c) lxor 1;
          dfs i (Some c))
    in
    dfs 1 None;
    parity
  in
  let parity = get_parity tree in
  let black =
    Array.filter_mapi parity ~f:(fun id x -> if x = 1 then Some id else None)
    |> Array.to_list
  in
  let white =
    Array.filter_mapi parity ~f:(fun id x -> if x = 0 then Some id else None)
    |> Array.to_list
  in
  let bi_edges =
    List.concat_map edges ~f:(fun (x, y) -> [ x, y; y, x ]) |> Pair_Set.of_list
  in
  let all_possibility = List.cartesian_product black white |> Pair_Set.of_list in
  let available = Set.diff all_possibility bi_edges in
  let rec play ?(first = false) available =
    let remove_edge st (x, y) =
      if x = -1 && y = -1
      then Error ()
      else (
        let st = Set.remove st (x, y) in
        Ok (Set.remove st (y, x)))
    in
    let available =
      if first
      then
        if Set.length available land 1 = 1
        then (
          print_endline "First";
          Ok available)
        else (
          print_endline "Second";
          let e = read_int_list () |> to_2ple in
          remove_edge available e)
      else Ok available
    in
    match available with
    | Ok available ->
      let x, y = Set.min_elt_exn available in
      let x, y = if x < y then x, y else y, x in
      printf "%d %d\n" x y;
      Out_channel.flush stdout;
      let available = remove_edge available (x, y) |> Result.ok |> Option.value_exn in
      let e = read_int_list () |> to_2ple in
      (match remove_edge available e with
       | Ok available -> play available
       | Error () -> ())
    | Error () -> ()
  in
  play ~first:true available
;;

(** End of file *)
