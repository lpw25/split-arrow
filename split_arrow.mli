module Arrow : sig
  module type S = sig
    type ('a, 'b) t

    val id : unit -> ('a, 'a) t
    val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
    val arr : ('a -> 'b) -> ('a, 'b) t
    val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  end
end

module Arrow_cartesian : sig
  module type S = sig
    type ('a, 'b) t

    val id : unit -> ('a, 'a) t
    val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
    val arr : ('a -> 'b) -> ('a, 'b) t
    val unit : unit -> ('a, unit) t
    val fst : unit -> ('a * 'b, 'a) t
    val snd : unit -> ('a * 'b, 'b) t
    val pair : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  end

  module Of_arrow (A : Arrow.S) : S with type ('a, 'b) t = ('a, 'b) A.t
end

module Simple : sig
  module type S = sig
    module Static : sig
      type 'a t

      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    module Dyn : sig
      type 'a t

      val const : 'a -> 'a t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val map : ('a -> 'b) -> 'a t -> 'b t Static.t
    end

    type ('a, 'b) t = 'a Dyn.t -> 'b Dyn.t Static.t
  end

  module Of_arrow_cartesian (X : Arrow_cartesian.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t

    exception Unbound_dynamic_value

    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow_cartesian (X : S) : sig
    include Arrow_cartesian.S

    val inject : ('a, 'b) X.t -> ('a, 'b) t
    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module Syntax (X : S) : sig
    val return : 'a -> 'a X.Static.t
    val ( let* ) : 'a X.Static.t -> ('a -> 'b X.Static.t) -> 'b X.Static.t
    val ( let+ ) : 'a X.Static.t -> ('a -> 'b) -> 'b X.Static.t
    val ( let& ) : 'a X.Dyn.t -> ('a -> 'b) -> 'b X.Dyn.t X.Static.t
    val ( and& ) : 'a X.Dyn.t -> 'b X.Dyn.t -> ('a * 'b) X.Dyn.t
  end
end

module Classified : sig
  module type S = sig
    module Static : sig
      type ('a, 'k) t

      val return : 'a -> ('a, 'k) t
      val bind : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t
    end

    module Dyn : sig
      type ('a, 'k) t

      val const : 'a -> ('a, 'k) t
      val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
      val map : ('a -> 'b) -> ('a, 'k) t -> (('b, 'k) t, 'k) Static.t
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Dyn.t -> (('b, 'k) Dyn.t, 'k) Static.t;
    }
  end

  module Of_arrow_cartesian (X : Arrow_cartesian.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t
    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow_cartesian (X : S) : sig
    include Arrow_cartesian.S

    val inject : ('a, 'b) X.t -> ('a, 'b) t
    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module Syntax (X : S) : sig
    val return : 'a -> ('a, 'k) X.Static.t

    val ( let* ) :
      ('a, 'k) X.Static.t -> ('a -> ('b, 'k) X.Static.t) -> ('b, 'k) X.Static.t

    val ( let+ ) : ('a, 'k) X.Static.t -> ('a -> 'b) -> ('b, 'k) X.Static.t

    val ( let& ) :
      ('a, 'k) X.Dyn.t -> ('a -> 'b) -> (('b, 'k) X.Dyn.t, 'k) X.Static.t

    val ( and& ) : ('a, 'k) X.Dyn.t -> ('b, 'k) X.Dyn.t -> ('a * 'b, 'k) X.Dyn.t
  end
end
