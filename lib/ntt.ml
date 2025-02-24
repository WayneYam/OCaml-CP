open! Base
open! Core
open! Stdio

module type NTT = sig
  val max_length : int

  type t = int array

  val transform_inplace : ?inverse:bool -> t -> unit
  val ( * ) : t -> t -> t
end

let make (prime : int) (primitive_root : int) (max_length : int) =
  (module struct
    let max_length = Int.ceil_pow2 max_length

    type t = int array

    let pow x y =
      let rec pow' acc cur_x cur_y =
        if cur_y = 0
        then acc
        else (
          let nxt_x = cur_x * cur_x % prime in
          let nxt_y = cur_y / 2 in
          let nxt_acc = if cur_y land 1 = 1 then acc * cur_x % prime else acc in
          pow' nxt_acc nxt_x nxt_y)
      in
      pow' 1 x y
    ;;

    let inv x = pow x (prime - 2)

    let w =
      let w = Array.init max_length ~f:(fun _ -> 1) in
      let dw = pow primitive_root ((prime - 1) / max_length) in
      for i = 1 to max_length - 1 do
        w.(i) <- w.(i - 1) * dw % prime
      done;
      w
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

    let transform_inplace ?(inverse = false) a =
      let sz = Array.length a in
      bitrev a;
      let l = ref 2 in
      while !l <= sz do
        let dx = max_length / !l in
        let dl = !l / 2 in
        for i' = 0 to (sz / !l) - 1 do
          let i = i' * !l in
          for j = i to i + dl - 1 do
            let tmp = a.(j + dl) * w.((j - i) * dx) % prime in
            a.(j + dl) <- a.(j) - tmp;
            if a.(j + dl) < 0 then a.(j + dl) <- a.(j + dl) + prime;
            a.(j) <- a.(j) + tmp;
            if a.(j) >= prime then a.(j) <- a.(j) - prime
          done
        done;
        l := !l * 2
      done;
      if inverse
      then (
        let invn = inv sz in
        Array.map_inplace ~f:(fun x -> x * invn % prime) a;
        for i = 1 to (sz / 2) - 1 do
          Array.swap a i (sz - i)
        done)
    ;;

    let ( * ) a b =
      let la = Array.length a in
      let lb = Array.length b in
      let deg = la + lb - 1 in
      let sz = Int.ceil_pow2 deg in
      let a' = Array.init sz ~f:(fun i -> if i < la then a.(i) else 0) in
      let b' = Array.init sz ~f:(fun i -> if i < lb then b.(i) else 0) in
      transform_inplace a';
      transform_inplace b';
      for i = 0 to sz - 1 do
        a'.(i) <- a'.(i) * b'.(i) % prime
      done;
      transform_inplace ~inverse:true a';
      Array.init deg ~f:(fun i -> a'.(i))
    ;;
  end : NTT)
;;
