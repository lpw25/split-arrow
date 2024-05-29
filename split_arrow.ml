module Category = struct
  module type S = sig
    type ('a, 'b) t

    val id : ('a, 'a) t
    val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  end
end

module Arrow = struct
  module type S = sig
    include Category.S

    val arr : ('a -> 'b) -> ('a, 'b) t
    val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    val second : ('b, 'c) t -> ('a * 'b, 'a * 'c) t

    val fst : ('a * 'b, 'a) t
    val snd : ('a * 'b, 'b) t
    val dup : ('a, 'a * 'a) t
  end
end

module Threadless = struct
  module type S = sig
    module Builder : sig
      type 'a t

      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    module Node : sig
      type 'a t

      val const : 'a -> 'a t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val fst : ('a * 'b) t -> 'a t
      val snd : ('a * 'b) t -> 'b t
      val map : ('a -> 'b) -> 'a t -> 'b t Builder.t
    end

    type ('a, 'b) t = 'a Node.t -> 'b Node.t Builder.t
  end

  module Syntax (X : S) = struct
    let return = X.Builder.return
    let ( let* ) = X.Builder.bind
    let ( let+ ) m f = X.Builder.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Node.map f a
    let ( and& ) = X.Node.both
  end

  module Key = Store.Key.Instance ()

  module Of_arrow (X : Arrow.S) = struct
    module Store = Store.Make (X)

    type 'a node =
      | Const : 'a -> 'a node
      | Loc : 'a Key.t -> 'a node
      | Both : 'a node * 'b node -> ('a * 'b) node
      | Fst : ('a * 'b) node -> 'a node
      | Snd : ('a * 'b) node -> 'b node

    type 'a builder =
      | Return : 'a -> 'a builder
      | Bind : 'a builder * ('a -> 'b builder) -> 'b builder
      | Inject : 'a node * ('a, 'b) X.t -> 'b node builder

    module Builder = struct
      type 'a t = 'a builder

      let return x = Return x
      let bind m f = Bind (m, f)
    end

    module Node = struct
      type 'a t = 'a node

      let const x = Const x
      let both t1 t2 = Both (t1, t2)
      let fst t = Fst t
      let snd t = Snd t
      let map f t = Inject (t, X.arr f)
    end

    type ('a, 'b) t = 'a Node.t -> 'b Node.t Builder.t

    let inject x v = Inject (v, x)

    type 'a state = State : ('a, 'b) X.t * ('b, Key.k) Store.t -> 'a state

    exception Unbound_node

    let extract f =
      let rec node : type a b. (a, Key.k) Store.t -> b Node.t -> (a, b) X.t =
       fun store d ->
        match d with
        | Const x -> X.arr (fun _ -> x)
        | Loc loc -> (
            match Store.find store loc with
            | None -> raise Unbound_node
            | Some arr -> arr)
        | Both (x, y) ->
            X.compose (X.compose (X.second (node store y))
                         (X.first (node store x)))
              X.dup
        | Fst x -> X.compose X.fst (node store x)
        | Snd x -> X.compose X.snd (node store x)
      in
      let rec builder : type a b. a state -> b Builder.t -> a state * b =
       fun (State (to_store, store) as s) m ->
        match m with
        | Return b -> (s, b)
        | Bind (m, f) ->
            let s, x = builder s m in
            builder s (f x)
        | Inject (d, arr) ->
            let darr = node store d in
            let arr = X.compose arr darr in
            let to_store =
              X.compose (X.second arr) (X.compose X.dup to_store)
            in
            let store =
              Store.map { f = (fun sarr -> X.compose sarr X.fst) } store
            in
            let loc = Key.create () in
            let store = Store.add store loc X.snd in
            let s = State (to_store, store) in
            (s, Loc loc)
      in
      let loc = Key.create () in
      let to_store = X.id in
      let store = Store.add Store.empty loc X.id in
      let s = State (to_store, store) in
      let State (to_store, store), d = builder s (f (Loc loc)) in
      X.compose (node store d) to_store
  end

  module To_arrow (X : S) = struct
    open X
    open Syntax (X)

    type ('a, 'b) t = ('a, 'b) X.t

    let id v = return v
    let compose t1 t2 v = Builder.bind (t2 v) t1

    let arr = Node.map

    let first t v =
      let+ v1 = t (Node.fst v) in
      Node.both v1 (Node.snd v)

    let second t v =
      let+ v2 = t (Node.snd v) in
      Node.both (Node.fst v) v2

    let fst v =
      return (Node.fst v)

    let snd v =
      return (Node.snd v)

    let dup v =
      return (Node.both v v)

  end
end

module Threaded = struct
  module type S = sig
    module Builder : sig
      type ('a, 'k) t

      val return : 'a -> ('a, 'k) t
      val bind : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t
    end

    module Node : sig
      type ('a, 'k) t

      val const : 'a -> ('a, 'k) t
      val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
      val fst : ('a * 'b, 'k) t -> ('a, 'k) t
      val snd : ('a * 'b, 'k) t -> ('b, 'k) t
      val map : ('a -> 'b) -> ('a, 'k) t -> (('b, 'k) t, 'k) Builder.t
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Node.t -> (('b, 'k) Node.t, 'k) Builder.t;
    }
  end

  module Syntax (X : S) = struct
    let return = X.Builder.return
    let ( let* ) = X.Builder.bind
    let ( let+ ) m f = X.Builder.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Node.map f a
    let ( and& ) = X.Node.both
  end

  module Of_arrow (X : Arrow.S) = struct
    module S = Store.Make (X)

    type ('a, 'k) node =
      | Const : 'a -> ('a, 'k) node
      | Loc : ('a, 'k) Store.Key.t -> ('a, 'k) node
      | Both : ('a, 'k) node * ('b, 'k) node -> ('a * 'b, 'k) node
      | Fst : ('a * 'b, 'k) node -> ('a, 'k) node
      | Snd : ('a * 'b, 'k) node -> ('b, 'k) node

    type ('a, 'k) builder =
      | Return : 'a -> ('a, 'k) builder
      | Bind : ('a, 'k) builder * ('a -> ('b, 'k) builder) -> ('b, 'k) builder
      | Inject : ('a, 'k) node * ('a, 'b) X.t -> (('b, 'k) node, 'k) builder

    module Builder = struct
      type ('a, 'k) t = ('a, 'k) builder

      let return x = Return x
      let bind m f = Bind (m, f)
    end

    module Node = struct
      type ('a, 'k) t = ('a, 'k) node

      let const x = Const x
      let both t1 t2 = Both (t1, t2)
      let fst t = Fst t
      let snd t = Snd t
      let map f t = Inject (t, X.arr f)
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Node.t -> (('b, 'k) Node.t, 'k) Builder.t;
    }

    let inject x = { poly = (fun v -> Inject (v, x)) }

    type ('a, 'k) state =
      | State : ('a, 'b) X.t * ('b, 'k) S.t -> ('a, 'k) state

    let extract f =
      let module Inst = Store.Key.Instance () in
      let rec node : type a b. (a, Inst.k) S.t -> (b, Inst.k) Node.t -> (a, b) X.t
          =
       fun store d ->
        match d with
        | Const x -> X.arr (fun _ -> x)
        | Loc loc -> (
            match S.find store loc with None -> assert false | Some arr -> arr)
        | Both (x, y) ->
            X.compose (X.compose (X.second (node store y))
                         (X.first (node store x)))
              X.dup
        | Fst x -> X.compose X.fst (node store x)
        | Snd x -> X.compose X.snd (node store x)
      in
      let rec builder :
          type a b.
          (a, Inst.k) state -> (b, Inst.k) Builder.t -> (a, Inst.k) state * b =
       fun (State (to_store, store) as s) m ->
        match m with
        | Return b -> (s, b)
        | Bind (m, f) ->
            let s, x = builder s m in
            builder s (f x)
        | Inject (d, arr) ->
            let darr = node store d in
            let arr = X.compose arr darr in
            let to_store =
              X.compose (X.second arr) (X.compose X.dup to_store)
            in
            let store =
              S.map { f = (fun sarr -> X.compose sarr X.fst) } store
            in
            let loc = Inst.create () in
            let store = S.add store loc X.snd in
            let s = State (to_store, store) in
            (s, Loc loc)
      in
      let loc = Inst.create () in
      let to_store = X.id in
      let store = S.add S.empty loc X.id in
      let s = State (to_store, store) in
      let State (to_store, store), d = builder s (f.poly (Loc loc)) in
      X.compose (node store d) to_store
  end

  module To_arrow (X : S) = struct
    open X
    open Syntax (X)

    type ('a, 'b) t = ('a, 'b) X.t

    let id = { poly = (fun v -> return v) }

    let compose { poly = poly1 } { poly = poly2 } =
      { poly = (fun v -> Builder.bind (poly2 v) poly1) }

    let arr f =
      { poly = (fun v -> Node.map f v) }

    let first { poly } =
      { poly =
          (fun v ->
            let+ v1 = poly (Node.fst v) in
            Node.both v1 (Node.snd v))
      }

    let second { poly } =
      { poly =
          (fun v ->
            let+ v2 = poly (Node.snd v) in
            Node.both (Node.fst v) v2)
      }

    let fst =
      { poly = (fun v -> return (Node.fst v)) }

    let snd =
      { poly = (fun v -> return (Node.snd v)) }

    let dup =
      { poly = (fun v -> return (Node.both v v)) }

  end
end
