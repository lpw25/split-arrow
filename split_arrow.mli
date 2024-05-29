module Category : sig
  module type S = sig
    type ('a, 'b) t

    val id : ('a, 'a) t
    val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t

    (** Should satisfy the following laws:

        - [compose (id ()) f = f]
        - [compose f (id ()) = f]
        - [compose (compse f g) h = compose f (compose g h)]
    *)
  end
end

module Arrow : sig
  module type S = sig
    include Category.S

    val arr : ('a -> 'b) -> ('a, 'b) t
    val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    val second : ('b, 'c) t -> ('a * 'b, 'a * 'c) t
    val fst : ('a * 'b, 'a) t
    val snd : ('a * 'b, 'b) t
    val dup : ('a, 'a * 'a) t

    (** Should satisfy the following laws:

        - [id () = arr (fun x -> x)]
        - [compose (arr f) (arr g) = arr (fun x -> f (g x))]
        - [first (arr f) = arr (fun (x, y) -> (f x, y))]
        - [first (compose f g) = compose (first f) (first g)]
        - [compose (arr (fun (x, y) -> x)) (first f)
           = compose f (arr (fun (x, y) -> x))]
        - [compose (arr (fun (x, y) -> (x, g y))) (first f)
           = compose (first f) (arr (fun (x, y) -> (x, g y)))]
        - [compose (arr (fun ((x, y), z) -> (x, (y, z)))) (first (first f))
           = compose (first f) (arr (fun ((x, y), z) -> (x, (y, z))))]
        - [second f =
             compose (arr (fun (y, x) -> (x, y)))
               (compose (first f) (arr (fun (x, y) -> (y, x))))]
        - [fst () = arr (fun (x, y) -> x)]
        - [snd () = arr (fun (x, y) -> y)]
        - [dup () = arr (fun x -> (x, x))]
    *)
  end
end

module Threadless : sig
  module type S = sig
    module Static : sig
      type 'a t

      val return : 'a -> 'a t

      val bind : 'a t -> ('a -> 'b t) -> 'b t

      (** Should satisfy the following laws:

          - [Static.bind (Static.return x) f = f x]
          - [Static.bind m Static.return = m]
          - [Static.bind (Static.bind m f) g
             = Static.bind m (fun x -> Static.bind (f x) g)]
      *)
    end

    module Dyn : sig
      type 'a t

      val const : 'a -> 'a t

      val both : 'a t -> 'b t -> ('a * 'b) t
      val fst : ('a * 'b) t -> 'a t
      val snd : ('a * 'b) t -> 'b t

      val map : ('a -> 'b) -> 'a t -> 'b t Static.t

      (** Should satisfy the following laws:

          - [Dyn.map (fun x -> x) a = Static.return a]
          - [Static.bind (Dyn.map g a) (Dyn.map f)
             = Dyn.map (fun x -> f (g x)) a]
          - [Dyn.map (fun (x, y) -> y) (Dyn.both (const a) b)
             = Static.return b]
          - [Dyn.map (fun (x, y) -> x) (Dyn.both a (const b))
             = Static.return a]
          - [Dyn.map (fun ((x, y), z) -> (x, (y, z))) (Dyn.both (Dyn.both a b) c)
             = Dyn.both a (Dyn.both b c)]
          - [Dyn.map (fun (x, y) -> (f x, g y)) (Dyn.both a b)
             = Static.bind (Dyn.map f a) (fun x ->
                 Static.bind (Dyn.map g b) (fun y ->
                   Static.return (Dyn.both x y)))]
      *)
    end

    type ('a, 'b) t = 'a Dyn.t -> 'b Dyn.t Static.t

  end

  module Of_arrow (X : Arrow.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t

    exception Unbound_dynamic_value

    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow (X : S) : Arrow.S with type ('a, 'b) t = ('a, 'b) X.t

  module Syntax (X : S) : sig
    val return : 'a -> 'a X.Static.t

    val ( let* ) : 'a X.Static.t -> ('a -> 'b X.Static.t) -> 'b X.Static.t

    val ( let+ ) : 'a X.Static.t -> ('a -> 'b) -> 'b X.Static.t

    val ( let& ) : 'a X.Dyn.t -> ('a -> 'b) -> 'b X.Dyn.t X.Static.t

    val ( and& ) : 'a X.Dyn.t -> 'b X.Dyn.t -> ('a * 'b) X.Dyn.t
  end
end

module Threaded : sig
  module type S = sig
    module Static : sig
      type ('a, 'k) t

      val return : 'a -> ('a, 'k) t

      val bind : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t

      (** Should satisfy the following laws:

          - [Static.bind (Static.return x) f = f x]
          - [Static.bind m Static.return = m]
          - [Static.bind (Static.bind m f) g
             = Static.bind m (fun x -> Static.bind (f x) g)]
      *)
    end

    module Dyn : sig
      type ('a, 'k) t

      val const : 'a -> ('a, 'k) t

      val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
      val fst : ('a * 'b, 'k) t -> ('a, 'k) t
      val snd : ('a * 'b, 'k) t -> ('b, 'k) t

      val map : ('a -> 'b) -> ('a, 'k) t -> (('b, 'k) t, 'k) Static.t

      (** Should satisfy the following laws:

          - [Dyn.map (fun x -> x) a = Static.return a]
          - [Static.bind (Dyn.map g a) (Dyn.map f)
             = Dyn.map (fun x -> f (g x)) a]
          - [Dyn.map (fun (x, y) -> y) (Dyn.both (const a) b)
             = Static.return b]
          - [Dyn.map (fun (x, y) -> x) (Dyn.both a (const b))
             = Static.return a]
          - [Dyn.map (fun ((x, y), z) -> (x, (y, z))) (Dyn.both (Dyn.both a b) c)
             = Dyn.both a (Dyn.both b c)]
          - [Dyn.map (fun (x, y) -> (f x, g y)) (Dyn.both a b)
             = Static.bind (Dyn.map f a) (fun x ->
                 Static.bind (Dyn.map g b) (fun y ->
                   Static.return (Dyn.both x y)))]
      *)
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Dyn.t -> (('b, 'k) Dyn.t, 'k) Static.t;
    }
  end

  module Of_arrow (X : Arrow.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t

    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow (X : S) : Arrow.S with type ('a, 'b) t = ('a, 'b) X.t

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
