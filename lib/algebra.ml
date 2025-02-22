open! Base
open! Core
open! Stdio

module type Monoid = sig
  type t

  include Sexpable with type t := t

  val ( + ) : t -> t -> t
  val zero : t
end

module type Group = sig
  type t

  include Monoid with type t := t

  val ( ~- ) : t -> t
end

module type CommutativeMonoid = Monoid
module type CommutativeGroup = Group
