open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib.Input

let get_factor_list range =
  let res = Array.init (range + 1) ~f:(fun _ -> ([] : int list)) in
  for i = range downto 1 do
    for j = i + 1 to range / i do
      res.(i * j) <- i :: res.(i * j)
    done
  done;
  res
;;

let () =
  let range = 100000 in
  let factor_list = get_factor_list range in
  (* print_s @@ (sexp_of_array @@ sexp_of_list @@ sexp_of_int) factor_list; *)
  let _, q = to_2ple @@ read_int_list () in
  let array = read_int_list () in
  let queries =
    List.sort ~compare:[%compare: int * int * int * int]
    @@ List.init q ~f:(fun id ->
      let l, r, x = to_3ple @@ read_int_list () in
      l, r, x, q - 1 - id)
  in
  let next_occurence = Array.init (range + 1) ~f:(fun _ -> None) in
  let answer = Array.init q ~f:(fun _ -> false) in
  let rec calc_answer lst queries idx =
    match lst with
    | [] -> queries
    | hd :: tl ->
      let rest_queries = calc_answer tl queries (idx + 1) in
      next_occurence.(hd) <- Some idx;
      let test x r =
        match next_occurence.(x) with
        | None -> false
        | Some r' -> r' <= r
      in
      let solve_l x r qid =
        let factors = factor_list.(x) in
        answer.(qid) <- List.exists ~f:(fun t -> test t r && test (x / t) r) factors
      in
      let rec calc_l queries =
        match queries with
        | [] -> []
        | (l, r, x, qid) :: tl ->
          if l = idx
          then (
            solve_l x r qid;
            calc_l tl)
          else queries
      in
      calc_l rest_queries
  in
  calc_answer array (List.rev queries) 1 |> ignore;
  Array.iter ~f:(fun x -> print_string @@ if x then "YES\n" else "NO\n") answer
;;

(** End of file *)
