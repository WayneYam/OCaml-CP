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
  let _, r0, c0 = read_int_list () |> to_3ple in
  let get_vec = function
    | 'N' -> 1, 0
    | 'S' -> -1, 0
    | 'E' -> 0, -1
    | 'W' -> 0, 1
    | _ -> assert false
  in
  let moves = read_line () |> String.to_list |> List.map ~f:get_vec in
  moves
  |> List.fold_map
       ~init:(Pair_Set.of_list [ 0, 0 ], (0, 0), (r0, c0))
       ~f:(fun (st, (rf, cf), (rt, ct)) (dr, dc) ->
         let rf, rt = rf + dr, rt + dr in
         let cf, ct = cf + dc, ct + dc in
         let st = Set.add st (rf, cf) in
         let res = Set.mem st (rt, ct) in
         (st, (rf, cf), (rt, ct)), res)
  |> snd
  |> List.map ~f:Bool.to_int
  |> List.iter ~f:(printf "%d");
  printf "\n"
;;

(** End of file *)
