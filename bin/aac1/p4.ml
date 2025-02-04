open! Base
open! Core
open! Stdio

(** Definition of modules here *)

open! Lib.Input

module Pair_int = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module Pair_set = Set.Make (Pair_int)

let get_factor_list range =
  let res = Array.init (range + 1) ~f:(fun id -> ([] : int list)) in
  for i = range downto 1 do
    for j = i + 1 to range / i do
      res.(i * j) <- i :: res.(i * j)
    done
  done;
  res
;;

let () =
  let factor_list = get_factor_list 100000 in
  (* print_s @@ (sexp_of_array @@ sexp_of_list @@ sexp_of_int) factor_list; *)
  match read_int_list () with
  | [ n; q ] ->
    let array = read_int_list () in
    let pair_set = Pair_set.of_list @@ List.mapi ~f:(fun id v -> v, id + 1) array in
    let answer_query l r x =
      let memoi = Hashtbl.create (module Pair_int) in
      let test_value t =
        let search () =
          match
            Set.binary_search
              pair_set
              `First_greater_than_or_equal_to
              ~compare:[%compare: int * int]
              (t, l)
          with
          | None -> false
          | Some (v, id) -> if v = t && id <= r then true else false
        in
        match Hashtbl.find memoi (t, l) with
        | None ->
          let b = search () in
          Hashtbl.set memoi ~key:(t, l) ~data:b;
          b
        | Some b -> b
      in
      let test_factor t = test_value t && test_value (x / t) in
      List.exists ~f:test_factor factor_list.(x)
    in
    for i = 1 to q do
      match read_int_list () with
      | [ l; r; x ] -> print_string (if answer_query l r x then "YES\n" else "NO\n")
      | _ -> assert false
    done
  | _ -> assert false
;;

(** End of file *)
