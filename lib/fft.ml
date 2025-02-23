open! Base
open! Core
open! Stdio

module type FFT = sig
  val max_length : int

  type cp = Stdlib.Complex.t

  val sexp_of_cp : cp -> Sexp.t

  type t = cp array

  val transform_inplace : ?inv:bool -> t -> unit
  val ( * ) : t -> t -> t
end

let make (n : int) =
  (module struct
    let max_length = Int.ceil_pow2 n

    type cp = Stdlib.Complex.t

    let sexp_of_cp (x : cp) =
      Sexp.List [ sexp_of_float x.re; Sexp.Atom "+"; sexp_of_float x.im; Sexp.Atom "i" ]
    ;;

    type t = cp array

    let w =
      Array.init max_length ~f:(fun x ->
        (let angle = Float.(2. * pi * of_int x / of_int max_length) in
         { re = Float.cos angle; im = Float.sin angle }
         : cp))
    ;;

    let bitrev arr =
      let sz = Array.length arr in
      let rev_inc x =
        let rec aux x cur_bit =
          let y = x lxor cur_bit in
          if y >= x then y else aux y (cur_bit / 2)
        in
        aux x (sz / 2)
      in
      let i = ref 0 in
      for j = 1 to sz - 2 do
        i := rev_inc !i;
        if j < !i then Array.swap arr !i j
      done
    ;;

    let transform_inplace ?(inv = false) a =
      let sz = Array.length a in
      bitrev a;
      let process_block_size l =
        let dx = max_length / l in
        let dl = l / 2 in
        let process_block i =
          let rec loop j x =
            if j < i + dl
            then (
              let module C = Stdlib.Complex in
              let tmp = C.mul a.(j + dl) (if inv then C.conj w.(x) else w.(x)) in
              a.(j + dl) <- C.sub a.(j) tmp;
              a.(j) <- C.add a.(j) tmp;
              ();
              loop (j + 1) (x + dx))
          in
          loop i 0
        in
        let rec loop i =
          if i < sz
          then (
            process_block i;
            loop (i + l))
        in
        loop 0
      in
      let rec process bs =
        if bs <= sz
        then (
          process_block_size bs;
          process (bs * 2))
      in
      process 2;
      if inv
      then (
        let (invn : cp) = { re = Float.of_int sz; im = 0. } in
        Array.map_inplace ~f:(fun x -> Stdlib.Complex.(div x invn)) a)
    ;;

    let ( * ) a b =
      let a = Array.copy a in
      let b = Array.copy b in
      let la = Array.length a in
      let lb = Array.length b in
      let deg = la + lb - 1 in
      let sz = Int.ceil_pow2 deg in
      let a' =
        Array.init sz ~f:(fun i -> if i < la then a.(i) else Stdlib.Complex.zero)
      in
      let b' =
        Array.init sz ~f:(fun i -> if i < lb then b.(i) else Stdlib.Complex.zero)
      in
      transform_inplace a';
      transform_inplace b';
      for i = 0 to sz - 1 do
        a'.(i) <- Stdlib.Complex.mul a'.(i) b'.(i)
      done;
      transform_inplace ~inv:true a';
      Array.init deg ~f:(fun i -> a'.(i))
    ;;
  end : FFT)
;;
