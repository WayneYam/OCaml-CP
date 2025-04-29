open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let n, k = read_int_list () |> to_2ple in
  let str = read_line () in
  let str =
    String.init n ~f:(fun id ->
        match str.[id] with
        | '?' ->
            if
              (id > 0 && Char.(str.[id - 1] = 'o'))
              || (id < n - 1 && Char.(str.[id + 1] = 'o'))
            then '.'
            else '?'
        | x -> x)
  in
  let segments =
    str |> String.to_list
    |> List.group ~break:(fun x y -> not @@ Char.equal x y)
  in
  let groups =
    segments |> List.map ~f:(fun lst -> (List.hd_exn lst, List.length lst))
  in
  let min_k = String.count str ~f:(Char.equal 'o') in
  let max_k =
    List.sum
      (module Int)
      groups
      ~f:(fun (ch, cnt) -> if Char.equal ch '?' then (cnt + 1) / 2 else 0)
    + min_k
  in
  let res =
    if k = min_k then
      String.map str ~f:(fun c -> if Char.equal c '?' then '.' else c)
    else if k = max_k then
      groups
      |> List.map ~f:(fun (ch, cnt) ->
             if Char.equal ch '?' then
               if cnt % 2 = 1 then
                 String.init cnt ~f:(fun id -> if id % 2 = 1 then '.' else 'o')
               else String.init cnt ~f:(fun _ -> ch)
             else String.init cnt ~f:(fun _ -> ch))
      |> String.concat
    else str
  in
  print_endline res

(** End of file *)
