
[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    load_path = [];
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    transparent_modules = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies = []
  }]
let () =
  Ppx_module_timer_runtime.record_start Ppx_module_timer_runtime.__MODULE__
open! Base
open! Core
open! Stdio
module Lib =
  struct
    module Tree =
      struct
        open! Base[@@ocaml.doc
                    " \n    Still not quite sure how to design DFS.\n    \n    The tree should contain two types, one for node and one for edge.\n    \n    They should probably both satisfy some signatures so maybe I should make them both modules.\n    \n    Current idea is to have four parameters, type of node, type of edge, the function for pushing down, and the function to merge back up\n        "]
        open! Core
        open! Stdio
        type 'a node = {
          data: 'a ;
          adj: 'a node list }[@@deriving sexp]
        include
          struct
            let _ = fun (_ : 'a node) -> ()
            let rec node_of_sexp :
              'a . (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a node =
              let error_source__003_ = "bin/aac1/p4_submit.ml.Lib.Tree.node" in
              fun _of_a__001_ ->
                fun x__004_ ->
                  Sexplib0.Sexp_conv_record.record_of_sexp
                    ~caller:error_source__003_
                    ~fields:(Field
                               {
                                 name = "data";
                                 kind = Required;
                                 conv = _of_a__001_;
                                 rest =
                                   (Field
                                      {
                                        name = "adj";
                                        kind = Required;
                                        conv =
                                          (list_of_sexp
                                             (node_of_sexp _of_a__001_));
                                        rest = Empty
                                      })
                               })
                    ~index_of_field:(function
                                     | "data" -> 0
                                     | "adj" -> 1
                                     | _ -> (-1)) ~allow_extra_fields:false
                    ~create:(fun (data, (adj, ())) ->
                               ({ data; adj } : _ node)) x__004_
            let _ = node_of_sexp
            let rec sexp_of_node :
              'a . ('a -> Sexplib0.Sexp.t) -> 'a node -> Sexplib0.Sexp.t =
              fun _of_a__005_ ->
                fun { data = data__007_; adj = adj__009_ } ->
                  let bnds__006_ = ([] : _ Stdlib.List.t) in
                  let bnds__006_ =
                    let arg__010_ =
                      sexp_of_list (sexp_of_node _of_a__005_) adj__009_ in
                    ((Sexplib0.Sexp.List
                        [Sexplib0.Sexp.Atom "adj"; arg__010_])
                      :: bnds__006_ : _ Stdlib.List.t) in
                  let bnds__006_ =
                    let arg__008_ = _of_a__005_ data__007_ in
                    ((Sexplib0.Sexp.List
                        [Sexplib0.Sexp.Atom "data"; arg__008_])
                      :: bnds__006_ : _ Stdlib.List.t) in
                  Sexplib0.Sexp.List bnds__006_
            let _ = sexp_of_node
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        let tree_from_edges n edges =
          let nodes =
            Array.init (n + 1) ~f:(fun id -> { data = id; adj = [] }) in
          let rec add_edge =
            function
            | [] -> ()
            | (p, q)::tl ->
                (nodes.(p) <-
                   {
                     (nodes.(p)) with
                     adj = ((nodes.(q)) :: ((nodes.(p)).adj))
                   };
                 nodes.(q) <-
                   {
                     (nodes.(q)) with
                     adj = ((nodes.(p)) :: ((nodes.(q)).adj))
                   };
                 add_edge tl) in
          add_edge edges; nodes.(1)
      end
    module Input =
      struct
        open! Base
        open! Core
        open! Stdio
        let read_line () = let open In_channel in input_line_exn stdin
        let read_int () = Int.of_string @@ (read_line ())
        let read_int_list ?(on= [' '])  () =
          (List.map ~f:Int.of_string) @@
            ((String.split_on_chars ~on) @@ (read_line ()))
        let read_int64 () = Int64.of_string @@ (read_line ())
        let read_int64_list ?(on= [' '])  () =
          (List.map ~f:Int64.of_string) @@
            ((String.split_on_chars ~on) @@ (read_line ()))
        let to_2ple l = match l with | x::y::[] -> (x, y) | _ -> assert false
        let to_3ple l =
          match l with | x::y::z::[] -> (x, y, z) | _ -> assert false
        let to_4ple l =
          match l with | x::y::z::w::[] -> (x, y, z, w) | _ -> assert false
      end
  end[@@ocaml.doc " Template starts"][@@ocaml.doc " Template ends"]
open! Lib.Input
module Pair_int =
  struct
    type t = (int * int)[@@deriving (sexp, compare, hash)]
    include
      struct
        let _ = fun (_ : t) -> ()
        let t_of_sexp =
          (let error_source__017_ = "bin/aac1/p4_submit.ml.Pair_int.t" in
           function
           | Sexplib0.Sexp.List (arg0__012_::arg1__013_::[]) ->
               let res0__014_ = int_of_sexp arg0__012_
               and res1__015_ = int_of_sexp arg1__013_ in
               (res0__014_, res1__015_)
           | sexp__016_ ->
               Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                 error_source__017_ 2 sexp__016_ : Sexplib0.Sexp.t -> t)
        let _ = t_of_sexp
        let sexp_of_t =
          (fun (arg0__018_, arg1__019_) ->
             let res0__020_ = sexp_of_int arg0__018_
             and res1__021_ = sexp_of_int arg1__019_ in
             Sexplib0.Sexp.List [res0__020_; res1__021_] : t ->
                                                             Sexplib0.Sexp.t)
        let _ = sexp_of_t
        let compare =
          (fun a__022_ ->
             fun b__023_ ->
               let (t__024_, t__025_) = a__022_ in
               let (t__026_, t__027_) = b__023_ in
               match compare_int t__024_ t__026_ with
               | 0 -> compare_int t__025_ t__027_
               | n -> n : t -> ((t)[@merlin.hide ]) -> int)
        let _ = compare
        let (hash_fold_t :
          Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
          fun hsv ->
            fun arg ->
              let (e0, e1) = arg in
              let hsv = hash_fold_int hsv e0 in
              let hsv = hash_fold_int hsv e1 in hsv
        let _ = hash_fold_t
        let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
          let func arg =
            Ppx_hash_lib.Std.Hash.get_hash_value
              (let hsv = Ppx_hash_lib.Std.Hash.create () in
               hash_fold_t hsv arg) in
          fun x -> func x
        let _ = hash
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module Pair_set = (Set.Make)(Pair_int)
module Int_set = (Set.Make)(Int)
let get_factor_list range =
  let res = Array.init (range + 1) ~f:(fun _ -> ([] : int list)) in
  for i = range downto 1 do
    for j = i + 1 to range / i do res.(i * j) <- (i :: (res.(i * j))) done
  done;
  res
let () =
  let range = 100000 in
  let factor_list = get_factor_list range in
  let (_, q) = to_2ple @@ (read_int_list ()) in
  let array = read_int_list () in
  let queries =
    (List.sort
       ~compare:(fun (a__028_ : (int * int * int * int)) ->
                   fun (((b__029_ : (int * int * int * int)))[@merlin.hide ])
                     ->
                     ((let (t__030_, t__031_, t__032_, t__033_) = a__028_ in
                       let (t__034_, t__035_, t__036_, t__037_) = b__029_ in
                       match compare_int t__030_ t__034_ with
                       | 0 ->
                           (match compare_int t__031_ t__035_ with
                            | 0 ->
                                (match compare_int t__032_ t__036_ with
                                 | 0 -> compare_int t__033_ t__037_
                                 | n -> n)
                            | n -> n)
                       | n -> n)
                     [@merlin.hide ])))
      @@
      (List.init q
         ~f:(fun id ->
               let (l, r, x) = to_3ple @@ (read_int_list ()) in
               (l, r, x, ((q - 1) - id)))) in
  let next_occurence = Array.init (range + 1) ~f:(fun _ -> None) in
  let answer = Array.init q ~f:(fun _ -> false) in
  let rec calc_answer lst queries idx =
    match lst with
    | [] -> queries
    | hd::tl ->
        let rest_queries = calc_answer tl queries (idx + 1) in
        (next_occurence.(hd) <- (Some idx);
         (let test x r =
            match next_occurence.(x) with
            | None -> false
            | Some r' -> r' <= r in
          let solve_l x r qid =
            let factors = factor_list.(x) in
            answer.(qid) <-
              (List.exists ~f:(fun t -> (test t r) && (test (x / t) r))
                 factors) in
          let rec calc_l queries =
            match queries with
            | [] -> []
            | (l, r, x, qid)::tl ->
                if l = idx then (solve_l x r qid; calc_l tl) else queries in
          calc_l rest_queries)) in
  ignore (calc_answer array (List.rev queries) 1);
  Array.iter ~f:(fun x -> print_string @@ (if x then "YES\n" else "NO\n"))
    answer
[@@@ocaml.text " End of file "]
let () =
  Ppx_module_timer_runtime.record_until Ppx_module_timer_runtime.__MODULE__

