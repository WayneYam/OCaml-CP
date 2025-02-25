open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let n, q = read_int_list () |> to_2ple in
  let lst = read_int_list () in
  let queries =
    List.init q ~f:(fun id ->
      let r, x = read_int_list () |> to_2ple in
      r, x, q - 1 - id)
    |> List.sort ~compare:[%compare: int * int * int]
  in
  let inf = 2000000000 in
  let dp = Array.init n ~f:(fun _ -> inf) in
  let ans = Array.init q ~f:(fun _ -> 0) in
  let rec add id lst qs =
    match lst with
    | [] -> ()
    | hd :: tl ->
      let pos =
        Array.binary_search dp `First_greater_than_or_equal_to hd ~compare
        |> Option.value_exn
      in
      dp.(pos) <- hd;
      let rec process_queries qs =
        match qs with
        | [] -> qs
        | (r, x, qid) :: tl ->
          if r <> id
          then qs
          else (
            let pos =
              Array.binary_search dp `Last_less_than_or_equal_to x ~compare
              |> Option.value_exn
            in
            ans.(qid) <- pos + 1;
            process_queries tl)
      in
      add (id + 1) tl (process_queries qs)
  in
  add 1 lst queries;
  Array.iter ans ~f:(fun x -> printf "%d\n" x)
;;

(** End of file *)
