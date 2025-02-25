open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let get_factor_list range =
  let res = Array.init (range + 1) ~f:(fun _ -> ([] : int list)) in
  for i = 1 to range do
    for j = i to range / i do
      res.(i * j) <- i :: res.(i * j)
    done
  done;
  res
;;

let maxN = 1000000

let () =
  let _, k = read_int_list () |> to_2ple in
  let lst = read_int_list () in
  let factor_list = get_factor_list maxN in
  let count = Array.init (maxN + 1) ~f:(fun _ -> 0) in
  let count_with_factor = Array.init (maxN + 1) ~f:(fun _ -> 0) in
  lst |> List.iter ~f:(fun x -> count.(x) <- count.(x) + 1);
  Array.iteri factor_list ~f:(fun id lst ->
    List.iter lst ~f:(fun x ->
      count_with_factor.(x) <- count_with_factor.(x) + count.(id);
      if id / x > x
      then count_with_factor.(id / x) <- count_with_factor.(id / x) + count.(id)));
  let answer =
    Array.init (maxN + 1) ~f:(fun i ->
      if i = 0
      then 0
      else (
        let ans1 =
          factor_list.(i)
          |> List.find ~f:(fun x -> count_with_factor.(x) >= k)
          |> Option.value ~default:0
        in
        let ans2 =
          factor_list.(i)
          |> List.rev
          |> List.find ~f:(fun x -> count_with_factor.(i / x) >= k)
          |> Option.value ~default:1000000000
          |> fun x -> i / x
        in
        max ans1 ans2))
  in
  List.iter lst ~f:(fun x -> printf "%d\n" answer.(x))
;;

(** End of file *)
