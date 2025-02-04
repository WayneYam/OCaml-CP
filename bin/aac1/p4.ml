open! Base
open! Core
open! Stdio

(** Definition of modules here *)

open! Lib.Input

module Pair_int = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module Pair_set = Set.Make (Pair_int)
module Int_set = Set.Make (Int)

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
    let positions = Array.init 100001 ~f:(fun id -> Int_set.empty) in
    List.iteri ~f:(fun id v -> positions.(v) <- Set.add positions.(v) (id + 1)) array;
    let answer_query l r x =
      let test_value t =
        let search () =
          match
            Set.binary_search
              positions.(t)
              `First_greater_than_or_equal_to
              ~compare:[%compare: int]
              l
          with
          | None -> false
          | Some id -> if id <= r then true else false
        in
        search ()
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
