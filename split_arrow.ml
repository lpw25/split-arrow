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
    module Static : sig
      type 'a t

      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    module Dyn : sig
      type 'a t

      val const : 'a -> 'a t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val fst : ('a * 'b) t -> 'a t
      val snd : ('a * 'b) t -> 'b t
      val map : ('a -> 'b) -> 'a t -> 'b t Static.t
    end

    type ('a, 'b) t = 'a Dyn.t -> 'b Dyn.t Static.t
  end

  module Syntax (X : S) = struct
    let return = X.Static.return
    let ( let* ) = X.Static.bind
    let ( let+ ) m f = X.Static.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Dyn.map f a
    let ( and& ) = X.Dyn.both
  end

  module Key = Store.Key.Instance ()

  module Of_arrow (X : Arrow.S) = struct
    module Store = Store.Make (X)

    type 'a dyn =
      | Const : 'a -> 'a dyn
      | Loc : 'a Key.t -> 'a dyn
      | Both : 'a dyn * 'b dyn -> ('a * 'b) dyn
      | Fst : ('a * 'b) dyn -> 'a dyn
      | Snd : ('a * 'b) dyn -> 'b dyn

    type 'a static =
      | Return : 'a -> 'a static
      | Bind : 'a static * ('a -> 'b static) -> 'b static
      | Inject : 'a dyn * ('a, 'b) X.t -> 'b dyn static

    module Static = struct
      type 'a t = 'a static

      let return x = Return x
      let bind m f = Bind (m, f)
    end

    module Dyn = struct
      type 'a t = 'a dyn

      let const x = Const x
      let both t1 t2 = Both (t1, t2)
      let fst t = Fst t
      let snd t = Snd t
      let map f t = Inject (t, X.arr f)
    end

    type ('a, 'b) t = 'a Dyn.t -> 'b Dyn.t Static.t

    let inject x v = Inject (v, x)

    type 'a state = State : ('a, 'b) X.t * ('b, Key.k) Store.t -> 'a state

    exception Unbound_dynamic_value

    let extract f =
      let rec dyn : type a b. (a, Key.k) Store.t -> b Dyn.t -> (a, b) X.t =
       fun store d ->
        match d with
        | Const x -> X.arr (fun _ -> x)
        | Loc loc -> (
            match Store.find store loc with
            | None -> raise Unbound_dynamic_value
            | Some arr -> arr)
        | Both (x, y) ->
            X.compose (X.compose (X.second (dyn store y))
                         (X.first (dyn store x)))
              X.dup
        | Fst x -> X.compose X.fst (dyn store x)
        | Snd x -> X.compose X.snd (dyn store x)
      in
      let rec static : type a b. a state -> b Static.t -> a state * b =
       fun (State (to_store, store) as s) m ->
        match m with
        | Return b -> (s, b)
        | Bind (m, f) ->
            let s, x = static s m in
            static s (f x)
        | Inject (d, arr) ->
            let darr = dyn store d in
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
      let State (to_store, store), d = static s (f (Loc loc)) in
      X.compose (dyn store d) to_store
  end

  module To_arrow (X : S) = struct
    open X
    open Syntax (X)

    type ('a, 'b) t = ('a, 'b) X.t

    let id v = return v
    let compose t1 t2 v = Static.bind (t2 v) t1

    let arr = Dyn.map

    let first t v =
      let+ v1 = t (Dyn.fst v) in
      Dyn.both v1 (Dyn.snd v)

    let second t v =
      let+ v2 = t (Dyn.snd v) in
      Dyn.both (Dyn.fst v) v2

    let fst v =
      return (Dyn.fst v)

    let snd v =
      return (Dyn.snd v)

    let dup v =
      return (Dyn.both v v)

  end
end

module Threaded = struct
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
      val fst : ('a * 'b, 'k) t -> ('a, 'k) t
      val snd : ('a * 'b, 'k) t -> ('b, 'k) t
      val map : ('a -> 'b) -> ('a, 'k) t -> (('b, 'k) t, 'k) Static.t
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Dyn.t -> (('b, 'k) Dyn.t, 'k) Static.t;
    }
  end

  module Syntax (X : S) = struct
    let return = X.Static.return
    let ( let* ) = X.Static.bind
    let ( let+ ) m f = X.Static.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Dyn.map f a
    let ( and& ) = X.Dyn.both
  end

  module Of_arrow (X : Arrow.S) = struct
    module S = Store.Make (X)

    type ('a, 'k) dyn =
      | Const : 'a -> ('a, 'k) dyn
      | Loc : ('a, 'k) Store.Key.t -> ('a, 'k) dyn
      | Both : ('a, 'k) dyn * ('b, 'k) dyn -> ('a * 'b, 'k) dyn
      | Fst : ('a * 'b, 'k) dyn -> ('a, 'k) dyn
      | Snd : ('a * 'b, 'k) dyn -> ('b, 'k) dyn

    type ('a, 'k) static =
      | Return : 'a -> ('a, 'k) static
      | Bind : ('a, 'k) static * ('a -> ('b, 'k) static) -> ('b, 'k) static
      | Inject : ('a, 'k) dyn * ('a, 'b) X.t -> (('b, 'k) dyn, 'k) static

    module Static = struct
      type ('a, 'k) t = ('a, 'k) static

      let return x = Return x
      let bind m f = Bind (m, f)
    end

    module Dyn = struct
      type ('a, 'k) t = ('a, 'k) dyn

      let const x = Const x
      let both t1 t2 = Both (t1, t2)
      let fst t = Fst t
      let snd t = Snd t
      let map f t = Inject (t, X.arr f)
    end

    type ('a, 'b) t = {
      poly : 'k. ('a, 'k) Dyn.t -> (('b, 'k) Dyn.t, 'k) Static.t;
    }

    let inject x = { poly = (fun v -> Inject (v, x)) }

    type ('a, 'k) state =
      | State : ('a, 'b) X.t * ('b, 'k) S.t -> ('a, 'k) state

    let extract f =
      let module Inst = Store.Key.Instance () in
      let rec dyn : type a b. (a, Inst.k) S.t -> (b, Inst.k) Dyn.t -> (a, b) X.t
          =
       fun store d ->
        match d with
        | Const x -> X.arr (fun _ -> x)
        | Loc loc -> (
            match S.find store loc with None -> assert false | Some arr -> arr)
        | Both (x, y) ->
            X.compose (X.compose (X.second (dyn store y))
                         (X.first (dyn store x)))
              X.dup
        | Fst x -> X.compose X.fst (dyn store x)
        | Snd x -> X.compose X.snd (dyn store x)
      in
      let rec static :
          type a b.
          (a, Inst.k) state -> (b, Inst.k) Static.t -> (a, Inst.k) state * b =
       fun (State (to_store, store) as s) m ->
        match m with
        | Return b -> (s, b)
        | Bind (m, f) ->
            let s, x = static s m in
            static s (f x)
        | Inject (d, arr) ->
            let darr = dyn store d in
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
      let State (to_store, store), d = static s (f.poly (Loc loc)) in
      X.compose (dyn store d) to_store
  end

  module To_arrow (X : S) = struct
    open X
    open Syntax (X)

    type ('a, 'b) t = ('a, 'b) X.t

    let id = { poly = (fun v -> return v) }

    let compose { poly = poly1 } { poly = poly2 } =
      { poly = (fun v -> Static.bind (poly2 v) poly1) }

    let arr f =
      { poly = (fun v -> Dyn.map f v) }

    let first { poly } =
      { poly =
          (fun v ->
            let+ v1 = poly (Dyn.fst v) in
            Dyn.both v1 (Dyn.snd v))
      }

    let second { poly } =
      { poly =
          (fun v ->
            let+ v2 = poly (Dyn.snd v) in
            Dyn.both (Dyn.fst v) v2)
      }

    let fst =
      { poly = (fun v -> return (Dyn.fst v)) }

    let snd =
      { poly = (fun v -> return (Dyn.snd v)) }

    let dup =
      { poly = (fun v -> return (Dyn.both v v)) }

  end
end
