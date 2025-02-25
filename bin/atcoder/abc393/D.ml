open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let _ = read_int () in
  let lst = read_line () |> String.to_list |> List.map ~f:(fun c -> Char.(c = '1')) in
  let total_count = List.count ~f:Fn.id lst in
  let sum_distance =
    lst
    |> List.filter_mapi ~f:(fun id b -> if b then Some id else None)
    |> List.sum (module Int) ~f:Fn.id
  in
  let choose2 x = x * (x - 1) / 2 in
  let initial_answer = sum_distance - choose2 total_count in
  let rec calc lst cur_answer left_count left_dist_sum right_count right_dist_sum =
    match lst with
    | [] -> cur_answer
    | hd :: tl ->
      if hd
      then
        calc tl cur_answer (left_count + 1) left_dist_sum (right_count - 1) right_dist_sum
      else (
        let left_dist_sum' = left_dist_sum + left_count in
        let right_dist_sum' = right_dist_sum - right_count in
        calc
          tl
          (min cur_answer (left_dist_sum' + right_dist_sum'))
          left_count
          left_dist_sum'
          right_count
          right_dist_sum')
  in
  printf "%d" @@ calc lst initial_answer 0 0 total_count initial_answer
;;

(** End of file *)
