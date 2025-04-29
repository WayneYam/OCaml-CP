open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let argmax arr =
  arr
  |> Array.foldi ~init:None ~f:(fun idx bst cur ->
         match bst with
         | None -> Some (idx, cur)
         | Some (_, bst') -> if cur > bst' then Some (idx, cur) else bst)
  |> Option.value_exn |> fst

let find_height n e =
  let adj = Array.init (n + 1) ~f:(fun _ -> []) in
  e
  |> List.concat_map ~f:(fun (x, y) -> [ (x, y); (y, x) ])
  |> List.iter ~f:(fun (x, y) -> adj.(x) <- y :: adj.(x));
  let get_dist st =
    let dist = Array.init (n + 1) ~f:(fun _ -> 0) in
    let rec dfs c p =
      List.iter adj.(c) ~f:(fun i ->
          if i <> p then (
            dist.(i) <- 1 + dist.(c);
            dfs i c))
    in
    dfs st (-1);
    dist
  in
  let dist1 = get_dist 1 in
  let endpoint_1 = argmax dist1 in
  let dist2 = get_dist endpoint_1 in
  let endpoint_2 = argmax dist2 in
  let dist3 = get_dist endpoint_2 in
  Array.map2_exn dist2 dist3 ~f:max

let maxN = 1 lsl 19

module M = (val Fft.make maxN : Fft.FFT)

let mul arr1 arr2 =
  let arr1 =
    Array.map arr1 ~f:(fun x -> ({ re = Float.of_int x; im = 0.0 } : M.cp))
  in
  let arr2 =
    Array.map arr2 ~f:(fun x -> ({ re = Float.of_int x; im = 0.0 } : M.cp))
  in
  let arr = M.(arr1 * arr2) in
  Array.map arr ~f:(fun { re; _ } -> Int.of_float @@ Float.round_nearest @@ re)

let () =
  let get_dat () =
    let n = read_int () in
    let e = List.init (n - 1) ~f:(fun _ -> read_int_list () |> to_2ple) in
    let dist = find_height n e in
    let v = Array.max_elt dist ~compare |> Option.value_exn in
    let poly = Array.init n ~f:(fun _ -> 0) in
    Array.iteri dist ~f:(fun idx x -> if idx > 0 then poly.(x) <- poly.(x) + 1);
    (v, poly)
  in
  let v1, poly1 = get_dat () in
  let v2, poly2 = get_dat () in
  let bst = max v1 v2 in
  let poly = mul poly1 poly2 in
  let ans =
    Array.foldi poly ~init:0 ~f:(fun id cur cnt ->
        let id' = max (id + 1) bst in
        cur + (id' * cnt))
  in
  printf "%d\n" ans

(** End of file *)
