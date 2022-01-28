module Eq : sig
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Data : sig
  module type S = sig
    type ('a, 'b) t
  end
end

module Key : sig
  type ('a, 'k) t

  module Instance () : sig
    type k
    type nonrec 'a t = ('a, k) t

    val create : unit -> 'a t
  end
end

module Make (Data : Data.S) : sig
  type ('a, 'k) t

  val empty : ('a, 'k) t
  val add : ('a, 'k) t -> ('b, 'k) Key.t -> ('a, 'b) Data.t -> ('a, 'k) t
  val find : ('a, 'k) t -> ('b, 'k) Key.t -> ('a, 'b) Data.t option

  type ('a, 'b) map = { f : 'c. ('a, 'c) Data.t -> ('b, 'c) Data.t } [@@unboxed]

  val map : ('a, 'b) map -> ('a, 'k) t -> ('b, 'k) t
end
