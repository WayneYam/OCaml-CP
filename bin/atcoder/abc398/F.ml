open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input
include Modint_prime.O1

module Rolling_hash = struct
  type t = int * int

  (* let length = fst *)
  (* let hash = snd *)
  let b = 127
  let (empty : t) = 0, 0
  let add_suffix (l, h) ch = l + 1, (h *% b) +% ch
  let add_prefix (l, h) ch = l + 1, h +% (ch *% (b ^% l))
  let ( <+ ) c h = add_prefix h c
  let ( +> ) h c = add_suffix h c
end

open Rolling_hash

let () =
  let input = read_line () in
  let s = input |> String.to_list |> List.rev in
  let a1 =
    snd
    @@ List.fold_map s ~init:empty ~f:(fun h c ->
      let res = Char.to_int c <+ h in
      res, res)
    |> List.mapi ~f:(fun i (_, h) -> i, h)
  in
  let a2 =
    snd
    @@ List.fold_map s ~init:empty ~f:(fun h c ->
      let res = h +> Char.to_int c in
      res, res)
    |> List.mapi ~f:(fun i (_, h) -> i, h)
  in
  let same =
    List.map2_exn a1 a2 ~f:(fun (_, h1) (i, h2) -> if h1 = h2 then Some i else None)
    |> List.filter_map ~f:Fn.id
    |> List.last_exn
  in
  let n = String.length input in
  let ans = String.prefix input (n - same - 1) |> String.rev in
  printf "%s%s\n" input ans
;;

(** End of file *)
