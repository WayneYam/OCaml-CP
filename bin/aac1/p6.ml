open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

module Pair_int = struct
  type t = int * int [@@deriving sexp, compare]
end

module M = struct
  module T = Map.Make (Int)
  module T2 = Map.Make (Pair_int)

  type t =
    { lower : int T.t (** a -> value *)
    ; higher : int T2.t (** a + b, a -> value *)
    ; bound : int
    }
  [@@deriving sexp]

  let init = { lower = T.empty; higher = T2.empty; bound = 0 }

  (** preserve the ordered-ness of the lower set *)
  let insert_lower_and_update (st : int T.t) (x, v) =
    let compare ~key ~data x =
      data |> ignore;
      compare key x
    in
    let add_and_update st (x, v) =
      let st =
        Map.update st x ~f:(fun o ->
          match o with
          | None -> v
          | Some v' -> max v v')
      in
      let v = Map.find_exn st x in
      let rec aux st =
        let nxt = Map.binary_search st ~compare `First_strictly_greater_than x in
        match nxt with
        | None -> st
        | Some (x', v') -> if v' < v then aux (Map.remove st x') else st
      in
      aux st
    in
    let prv = Map.binary_search st ~compare `Last_strictly_less_than x in
    match prv with
    | None -> add_and_update st (x, v)
    | Some (_, v') -> if v' >= v then st else add_and_update st (x, v)
  ;;

  let insert_higher (st : int T2.t) (x, y, v) =
    Map.update st (x, y) ~f:(fun o ->
      match o with
      | None -> v
      | Some v' -> max v v')
  ;;

  let insert (t : t) (x, y, v) =
    if x > t.bound
    then { t with higher = insert_higher t.higher (x, y, v) }
    else { t with lower = insert_lower_and_update t.lower (y, v) }
  ;;

  let update_bound (t : t) new_bound : t =
    let lo, hi = Map.split_lt_ge t.higher (new_bound + 1, 0) in
    { lower =
        (let add ~key ~data acc =
           let _, y = key in
           insert_lower_and_update acc (y, data)
         in
         Map.fold ~init:t.lower ~f:add lo)
    ; higher = hi
    ; bound = new_bound
    }
  ;;

  let query (t : t) x =
    let compare ~key ~data x =
      data |> ignore;
      compare key x
    in
    match Map.binary_search t.lower ~compare `Last_less_than_or_equal_to x with
    | None -> 0
    | Some (_, v') -> v'
  ;;
end

let solve arr =
  let arr = List.sort arr ~compare:[%compare: int * int] in
  (* print_s @@ [%sexp_of: (int * int) list] arr; *)
  let res =
    List.fold arr ~init:M.init ~f:(fun t (a, b) ->
      let x, y = a + b, a in
      let t = M.update_bound t y in
      let res = M.query t (a - b) in
      let t = M.insert t (x, y, res + 1) in
      (* print_s @@ M.sexp_of_t t; *)
      (* printf "%d\n" res; *)
      t)
  in
  let res = M.update_bound res (Int.max_value - 1) in
  (* print_s @@ M.sexp_of_t res; *)
  Map.max_elt_exn res.lower |> snd
;;

let () =
  let n = read_int () in
  let arr = List.init n ~f:(fun _ -> read_int_list () |> to_2ple) in
  solve arr |> printf "%d\n"
;;

(** End of file *)
