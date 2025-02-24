open! Base
open! Core
open! Stdio

module type Modint = sig
  type t

  val m : int
  val of_int : int -> t
  val to_int : t -> int
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( - ) : t -> t -> t
  val ( ~/ ) : t -> t
  val ( / ) : t -> t -> t
  val pow : t -> int -> t
end

(** Montgomery form for faster operations modulo P *)
let make (prime : int) =
  (module struct
    type t = int

    let m = prime
    let n = 1 lsl 32
    let n1 = Int.(n - 1)
    let n2 = n % m * n % m

    (** This doesn't work with primes above 2 ** 30 *)
    let () = assert (m < 1 lsl 30)

    let ( %% ) a b =
      let x = a % b in
      if x < 0 then x + b else x
    ;;

    let r =
      let rec get_r cur step =
        if step = 0
        then cur
        else (
          let nxt = (2 - (m * cur)) %% n * cur %% n in
          get_r nxt (step - 1))
      in
      let r = get_r m 4 in
      assert (r * m % n = 1);
      (1 lsl 32) - r
    ;;

    let reduce x =
      let y = Int.(x land n1 * r land n1) in
      let z = Int.((x + (y * m)) lsr 32) in
      let z = if Int.(z >= m) then Int.(z - m) else z in
      z
    ;;

    let of_int x = reduce Int.(x * n2)
    let to_int x = reduce x
    let ( ~- ) x = if x = 0 then x else Int.(m - x)

    let ( + ) x y =
      let z = x + y in
      if z >= m then z - m else z
    ;;

    let ( - ) x y = x + ~-y
    let ( * ) x y = reduce Int.(x * y)

    let ( ~/ ) x =
      let rec extgcd x y u v =
        if y = 0
        then u
        else (
          let t = Int.(x / y) in
          extgcd y Int.(x - (t * y)) v Int.(u - (t * v)))
      in
      of_int @@ extgcd (reduce x) m 1 0
    ;;

    let ( / ) x y = x * ~/y

    let pow x y =
      let rec pow' acc cur_x cur_y =
        if cur_y = 0
        then acc
        else (
          let nxt_x = cur_x * cur_x in
          let nxt_y = Int.(cur_y / 2) in
          let nxt_acc = if Int.(cur_y land 1) = 1 then acc * cur_x else acc in
          pow' nxt_acc nxt_x nxt_y)
      in
      pow' (of_int 1) x y
    ;;
  end : Modint)
;;
