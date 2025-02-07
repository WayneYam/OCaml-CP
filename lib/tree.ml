open! Base
open! Core
open! Stdio

type ('a, 'b) node =
  { id : int
  ; data : 'a
  ; mutable adj : ('b * ('a, 'b) node) list
  }

let init ~nodes ~edges =
  let nodes =
    Array.of_list_mapi ~f:(fun id data -> ({ id; data; adj = [] } : ('a, 'b) node)) nodes
  in
  let rec add_edge = function
    | [] -> ()
    | (u, v, w) :: tl ->
      nodes.(u).adj <- (w, nodes.(v)) :: nodes.(u).adj;
      nodes.(v).adj <- (w, nodes.(u)) :: nodes.(v).adj;
      add_edge tl
  in
  add_edge edges;
  nodes.(1)
;;
