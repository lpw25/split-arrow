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
    module Builder : sig
      type 'a t

      val return : 'a -> 'a t

      val bind : 'a t -> ('a -> 'b t) -> 'b t

      (** Should satisfy the following laws:

          - [Builder.bind (Builder.return x) f = f x]
          - [Builder.bind m Builder.return = m]
          - [Builder.bind (Builder.bind m f) g
             = Builder.bind m (fun x -> Builder.bind (f x) g)]
      *)
    end

    module Node : sig
      type 'a t

      val const : 'a -> 'a t

      val both : 'a t -> 'b t -> ('a * 'b) t
      val fst : ('a * 'b) t -> 'a t
      val snd : ('a * 'b) t -> 'b t

      val map : ('a -> 'b) -> 'a t -> 'b t Builder.t

      (** Should satisfy the following laws:

          - [Node.map (fun x -> x) a = Builder.return a]
          - [Builder.bind (Node.map g a) (Node.map f)
             = Node.map (fun x -> f (g x)) a]
          - [Node.map (fun (x, y) -> y) (Node.both (const a) b)
             = Builder.return b]
          - [Node.map (fun (x, y) -> x) (Node.both a (const b))
             = Builder.return a]
          - [Node.map (fun ((x, y), z) -> (x, (y, z))) (Node.both (Node.both a b) c)
             = Node.both a (Node.both b c)]
          - [Node.map (fun (x, y) -> (f x, g y)) (Node.both a b)
             = Builder.bind (Node.map f a) (fun x ->
                 Builder.bind (Node.map g b) (fun y ->
                   Builder.return (Node.both x y)))]
      *)
    end

    type ('a, 'b) t = 'a Node.t -> 'b Node.t Builder.t

  end

  module Of_arrow (X : Arrow.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t

    exception Unbound_node

    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow (X : S) : Arrow.S with type ('a, 'b) t = ('a, 'b) X.t

  module Syntax (X : S) : sig
    val return : 'a -> 'a X.Builder.t

    val ( let* ) : 'a X.Builder.t -> ('a -> 'b X.Builder.t) -> 'b X.Builder.t

    val ( let+ ) : 'a X.Builder.t -> ('a -> 'b) -> 'b X.Builder.t

    val ( let& ) : 'a X.Node.t -> ('a -> 'b) -> 'b X.Node.t X.Builder.t

    val ( and& ) : 'a X.Node.t -> 'b X.Node.t -> ('a * 'b) X.Node.t
  end
end

module Threaded : sig
  module type S = sig
    module Builder : sig
      type ('a, 'k) t

      val return : 'a -> ('a, 'k) t

      val bind : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t

      (** Should satisfy the following laws:

          - [Builder.bind (Builder.return x) f = f x]
          - [Builder.bind m Builder.return = m]
          - [Builder.bind (Builder.bind m f) g
             = Builder.bind m (fun x -> Builder.bind (f x) g)]
      *)
    end

    module Node : sig
      type ('a, 'k) t

      val const : 'a -> ('a, 'k) t

      val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
      val fst : ('a * 'b, 'k) t -> ('a, 'k) t
      val snd : ('a * 'b, 'k) t -> ('b, 'k) t

      val map : ('a -> 'b) -> ('a, 'k) t -> (('b, 'k) t, 'k) Builder.t

      (** Should satisfy the following laws:

          - [Node.map (fun x -> x) a = Builder.return a]
          - [Builder.bind (Node.map g a) (Node.map f)
             = Node.map (fun x -> f (g x)) a]
          - [Node.map (fun (x, y) -> y) (Node.both (const a) b)
             = Builder.return b]
          - [Node.map (fun (x, y) -> x) (Node.both a (const b))
             = Builder.return a]
          - [Node.map (fun ((x, y), z) -> (x, (y, z))) (Node.both (Node.both a b) c)
             = Node.both a (Node.both b c)]
          - [Node.map (fun (x, y) -> (f x, g y)) (Node.both a b)
             = Builder.bind (Node.map f a) (fun x ->
                 Builder.bind (Node.map g b) (fun y ->
                   Builder.return (Node.both x y)))]
      *)
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Node.t -> (('b, 'k) Node.t, 'k) Builder.t;
    }
  end

  module Of_arrow (X : Arrow.S) : sig
    include S

    val inject : ('a, 'b) X.t -> ('a, 'b) t

    val extract : ('a, 'b) t -> ('a, 'b) X.t
  end

  module To_arrow (X : S) : Arrow.S with type ('a, 'b) t = ('a, 'b) X.t

  module Syntax (X : S) : sig
    val return : 'a -> ('a, 'k) X.Builder.t

    val ( let* ) :
      ('a, 'k) X.Builder.t -> ('a -> ('b, 'k) X.Builder.t) -> ('b, 'k) X.Builder.t

    val ( let+ ) : ('a, 'k) X.Builder.t -> ('a -> 'b) -> ('b, 'k) X.Builder.t

    val ( let& ) :
      ('a, 'k) X.Node.t -> ('a -> 'b) -> (('b, 'k) X.Node.t, 'k) X.Builder.t

    val ( and& ) : ('a, 'k) X.Node.t -> ('b, 'k) X.Node.t -> ('a * 'b, 'k) X.Node.t
  end
end
