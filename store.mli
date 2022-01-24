module Eq : sig
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Data : sig
  module type S = sig
    type ('a, 'b) t
  end
end

module Simple : sig
  module Make (Data : Data.S) : sig
    type 'a t

    module Key : sig
      type 'a t

      val equal : 'a t -> 'b t -> ('a, 'b) Eq.t option
      val create : unit -> 'a t
    end

    val empty : 'a t
    val add : 'a t -> 'b Key.t -> ('a, 'b) Data.t -> 'a t
    val find : 'a t -> 'b Key.t -> ('a, 'b) Data.t option

    type ('a, 'b) map = { f : 'c. ('a, 'c) Data.t -> ('b, 'c) Data.t }
    [@@unboxed]

    val map : ('a, 'b) map -> 'a t -> 'b t
  end
end

module Classified : sig
  module Make (Data : Data.S) : sig
    type ('a, 'k) t

    module Key : sig
      type ('a, 'k) t

      val equal : ('a, 'k) t -> ('b, 'k) t -> ('a, 'b) Eq.t option

      module Instance () : sig
        type k

        val create : unit -> ('a, k) t
      end
    end

    val empty : ('a, 'k) t
    val add : ('a, 'k) t -> ('b, 'k) Key.t -> ('a, 'b) Data.t -> ('a, 'k) t
    val find : ('a, 'k) t -> ('b, 'k) Key.t -> ('a, 'b) Data.t option

    type ('a, 'b) map = { f : 'c. ('a, 'c) Data.t -> ('b, 'c) Data.t }
    [@@unboxed]

    val map : ('a, 'b) map -> ('a, 'k) t -> ('b, 'k) t
  end
end
