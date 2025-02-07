(** 
Still not quite sure how to design DFS.

The tree should contain two types, one for node and one for edge.

They should probably both satisfy some signatures so maybe I should make them both modules.

Current idea is to have four parameters, type of node, type of edge, the function for pushing down, and the function to merge back up
    *)
open! Base

open! Core
open! Stdio

type 'a node =
  { data : 'a
  ; adj : 'a node list
  }
[@@deriving sexp]

let tree_from_edges n edges =
  let nodes = Array.init (n + 1) ~f:(fun id -> { data = id; adj = [] }) in
  let rec add_edge = function
    | [] -> ()
    | (p, q) :: tl ->
      nodes.(p) <- { (nodes.(p)) with adj = nodes.(q) :: nodes.(p).adj };
      nodes.(q) <- { (nodes.(q)) with adj = nodes.(p) :: nodes.(q).adj };
      add_edge tl
  in
  add_edge edges;
  nodes.(1)
;;
