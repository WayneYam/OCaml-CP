open! Base
open! Core
open! Stdio

module Algebra = struct
  module Group = struct
    type 'a t =
      { id : 'a
      ; plus : 'a -> 'a -> 'a
      ; inverse : 'a -> 'a
      ; sexp_of_t : 'a -> Sexp.t
      }

    let int_plus : int t = Int.{ id = zero; plus = ( + ); sexp_of_t; inverse = ( ~- ) }

    let xor : int t =
      Int.{ id = zero; plus = ( lxor ); sexp_of_t; inverse = (fun x -> x) }
    ;;

    let float_plus : float t =
      Float.{ id = one; plus = ( + ); sexp_of_t; inverse = ( ~- ) }
    ;;

    let float_mul : float t =
      Float.{ id = one; plus = ( * ); sexp_of_t; inverse = (fun x -> 1. / x) }
    ;;

    let minus g a b = g.plus a (g.inverse b)
  end
end

(** Manually including modules is needed because my expander sucks *)

type 'a t =
  { sum : 'a array
  ; data : 'a array
  ; op : 'a Algebra.Group.t
  }

let create ?(op : 'a Algebra.Group.t = Algebra.Group.int_plus) n : 'a t =
  { sum = Array.init ~f:(fun _ -> op.id) (n + 1)
  ; op
  ; data = Array.init ~f:(fun _ -> op.id) n
  }
;;

let len (tree : 'a t) = Array.length tree.sum
let low_bit x = x land -x

let add tree ~pos ~v =
  tree.data.(pos) <- tree.op.plus tree.data.(pos) v;
  let pos = pos + 1 in
  let rec add' pos =
    if pos < Array.length tree.sum
    then (
      tree.sum.(pos) <- tree.op.plus tree.sum.(pos) v;
      add' (pos + low_bit pos))
    else ()
  in
  add' pos
;;

let set tree ~pos ~v =
  let v = Algebra.Group.minus tree.op v tree.data.(pos) in
  add tree ~pos ~v
;;

let prefix_sum tree r =
  let rec get acc p =
    if p <= 0 then acc else get (tree.op.plus acc tree.sum.(p)) (p - low_bit p)
  in
  get tree.op.id r
;;

let sum tree l r = Algebra.Group.minus tree.op (prefix_sum tree r) (prefix_sum tree l)

let find_last_satisfying (tree : 'a t) pred =
  if not @@ pred tree.op.id
  then None
  else (
    let len = len tree in
    let rec search acc pos step =
      if step = 0
      then pos
      else (
        let pos' = pos + step in
        if pos' >= len
        then search acc pos (step / 2)
        else (
          let acc' = tree.op.plus acc tree.sum.(pos') in
          if pred acc' then search acc' pos' (step / 2) else search acc pos (step / 2)))
    in
    Some (search tree.op.id 0 (Int.floor_pow2 len)))
;;

let find_first_satisfying (tree : 'a t) pred =
  (* The first satisfying is the one just after the last not satisfying *)
  match find_last_satisfying tree (fun x -> not @@ pred x) with
  | None -> Some 0
  | Some x when x = len tree - 1 -> None
  | Some x -> Some (x + 1)
;;

let binary_search (tree : 'a t) ~compare how v =
  match how with
  | `Last_strictly_less_than -> find_last_satisfying tree (fun x -> compare x v < 0)
  | `Last_less_than_or_equal_to -> find_last_satisfying tree (fun x -> compare x v <= 0)
  | `First_equal_to ->
    (match find_first_satisfying tree (fun x -> compare x v >= 0) with
     | Some x when compare (prefix_sum tree x) v = 0 -> Some x
     | None | Some _ -> None)
  | `Last_equal_to ->
    (match find_last_satisfying tree (fun x -> compare x v <= 0) with
     | Some x when compare (prefix_sum tree x) v = 0 -> Some x
     | None | Some _ -> None)
  | `First_greater_than_or_equal_to ->
    find_first_satisfying tree (fun x -> compare x v >= 0)
  | `First_strictly_greater_than -> find_first_satisfying tree (fun x -> compare x v > 0)
;;
