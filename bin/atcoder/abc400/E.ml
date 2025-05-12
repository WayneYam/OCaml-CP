open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let prime_sieve n =
  let is_prime = Array.init (n + 1) ~f:(fun _ -> true) in
  let prime_factor = Array.init (n + 1) ~f:(fun _ -> -1) in
  for i = 2 to n do
    if is_prime.(i) then prime_factor.(i) <- i;
    for j = 2 to n / i do
      is_prime.(i * j) <- false;
      prime_factor.(i * j) <- j
    done
  done;
  prime_factor
;;

let int_square_root k =
  if k < 0
  then None
  else
    Binary_search.binary_search
      ~get:(fun () id -> id)
      ~length:(fun () -> min (k + 1) 2000000000)
      ()
      ~compare:(fun x target -> compare (x * x) target)
      `Last_less_than_or_equal_to
      k
;;

let () =
  let n = 1000000 in
  let prime_factor = prime_sieve n in
  let answer =
    let distinct_prime_factors = Array.init (n + 1) ~f:(fun _ -> 0) in
    for i = 2 to n do
      let pi = prime_factor.(i) in
      let diff = if pi = i || pi <> prime_factor.(i / pi) then 1 else 0 in
      distinct_prime_factors.(i) <- distinct_prime_factors.(i / pi) + diff
    done;
    distinct_prime_factors
    |> Array.map ~f:(fun x -> x = 2)
    |> Array.folding_mapi ~init:(-1) ~f:(fun id res cur ->
      if cur then id, id else res, res)
  in
  let solve x =
    let x = int_square_root x |> Option.value_exn in
    let x = answer.(x) in
    x * x
  in
  let q = read_int () in
  q
  |> List.init ~f:(fun _ -> read_int ())
  |> List.rev
  |> List.map ~f:solve
  |> List.iter ~f:(printf "%d\n")
;;

(** End of file *)
