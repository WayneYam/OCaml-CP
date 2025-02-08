open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

type in_data = { depth : int }

type out_data =
  { odd : int
  ; even : int
  }

let () =
  let n = read_int () in
  let edges =
    let rec get_edges acc c =
      if c = 0
      then acc
      else
        get_edges
          ((read_int_list () |> to_3ple |> fun (u, v, w) -> u, v, w % 2) :: acc)
          (c - 1)
    in
    get_edges [] (n - 1)
  in
  let tree = Tree.init (List.init (n + 1) ~f:ignore) edges in
  let init_in : in_data = { depth = 0 } in
  let init_out (data : in_data) : out_data =
    if data.depth % 2 = 1 then { odd = 1; even = 0 } else { odd = 0; even = 1 }
  in
  let merge_child (acc : out_data) (ch : out_data) : out_data =
    { odd = acc.odd + ch.odd; even = acc.even + ch.even }
  in
  let propagate (fa : in_data) w : in_data = { depth = (fa.depth + w) % 2 } in
  let rec dfs fa_id data (node : ('a, 'b) Tree.node) : (out_data, 'd) Tree.node =
    let child_result =
      List.filter_map
        ~f:(fun (w, v) ->
          if v.id = fa_id then None else Some ((), dfs node.id (propagate data w) v))
        node.adj
    in
    { id = node.id
    ; data =
        List.fold
          ~init:(init_out data)
          ~f:(fun acc (_, ch) -> merge_child acc ch.data)
          child_result
    ; adj = child_result
    }
  in
  let result = dfs 0 init_in tree in
  let total_odd, total_even =
    Int64.of_int result.data.odd, Int64.of_int result.data.even
  in
  let calc odd even =
    let open Int64 in
    let n = odd + even in
    abs ((n * (n - 1L) / 2L) - (2L * odd * even))
  in
  let rec dfs (node : (out_data, unit) Tree.node) =
    let open Int64 in
    let cur_odd, cur_even = of_int node.data.odd, of_int node.data.even in
    let swap_odd = total_odd - cur_odd + cur_even in
    let swap_even = total_even - cur_even + cur_odd in
    let child_result = List.unzip node.adj |> snd |> List.map ~f:dfs in
    List.fold ~init:(calc swap_odd swap_even) ~f:min child_result
  in
  printf "%s\n" @@ Int64.to_string (dfs result)
;;

(** End of file *)
