open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let gen_all_subset lst =
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | hd :: tl -> aux (List.map acc ~f:(fun lst -> hd :: lst) @ acc) tl
  in
  aux [ [] ] lst
;;

let gen_all_permutations lst =
  let split lst =
    let rec aux acc front back =
      match back with
      | [] -> acc
      | hd :: tl ->
        let acc = (hd, front @ tl) :: acc in
        let front = hd :: front in
        aux acc front tl
    in
    aux [] [] lst
  in
  let rec aux acc =
    match snd @@ List.hd_exn acc with
    | [] -> List.map ~f:fst acc
    | _ ->
      let res =
        List.concat_map acc ~f:(fun (perm, no_perm) ->
          let splitted = split no_perm in
          List.map splitted ~f:(fun (hd, tl) -> hd :: perm, tl))
      in
      aux res
  in
  aux [ [], lst ]
;;

let () =
  let n, m = read_int_list () |> to_2ple in
  let edges = List.init m ~f:(fun _ -> read_int_list () |> to_3ple) in
  let weight = Array.init (n + 1) ~f:(fun _ -> Array.init (n + 1) ~f:(fun _ -> None)) in
  List.iter edges ~f:(fun (u, v, w) ->
    weight.(u).(v) <- Some w;
    weight.(v).(u) <- Some w);
  let tests =
    List.init (n - 2) ~f:(fun id -> id + 2)
    |> gen_all_subset
    |> List.concat_map ~f:gen_all_permutations
    |> List.map ~f:(fun lst -> lst @ [ n ])
  in
  let res =
    List.filter_map tests ~f:(fun lst ->
      let rec calc_xor acc hd tl =
        match tl with
        | [] -> Some acc
        | hd2 :: tl2 ->
          (match weight.(hd).(hd2) with
           | None -> None
           | Some w -> calc_xor (w lxor acc) hd2 tl2)
      in
      calc_xor 0 1 lst)
    |> List.min_elt ~compare
    |> Option.value_exn
  in
  printf "%d\n" res
;;

(** End of file *)
