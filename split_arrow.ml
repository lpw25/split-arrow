module Category = struct
  module type S = sig
    type ('a, 'b) t

    val id : unit -> ('a, 'a) t
    val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  end
end

module Arrow = struct
  module type S = sig
    include Category.S

    val arr : ('a -> 'b) -> ('a, 'b) t
    val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  end
end

module Arrow_cartesian = struct
  module type S = sig
    include Category.S

    val arr : ('a -> 'b) -> ('a, 'b) t
    val unit : unit -> ('a, unit) t
    val fst : unit -> ('a * 'b, 'a) t
    val snd : unit -> ('a * 'b, 'b) t
    val pair : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  end

  module Of_arrow (A : Arrow.S) : S with type ('a, 'b) t = ('a, 'b) A.t = struct
    include A

    let unit () = arr (fun _ -> ())
    let fst () = arr fst
    let snd () = arr snd

    let second f =
      compose
        (arr (fun (x, y) -> (y, x)))
        (compose (first f) (arr (fun (x, y) -> (y, x))))

    let pair f s =
      compose (compose (first f) (second s)) (arr (fun x -> (x, x)))
  end

  module To_arrow (A : S) : Arrow.S with type ('a, 'b) t = ('a, 'b) A.t = struct
    include A

    let first f =
      pair (compose f (fst ())) (snd ())
  end
end

module Simple = struct
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

  module Syntax (X : S) = struct
    let return = X.Static.return
    let ( let* ) = X.Static.bind
    let ( let+ ) m f = X.Static.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Dyn.map f a
    let ( and& ) = X.Dyn.both
  end

  module Key = Store.Key.Instance ()

  module Of_arrow_cartesian (X : Arrow_cartesian.S) = struct
    module Store = Store.Make (X)

    type 'a dyn =
      | Const : 'a -> 'a dyn
      | Loc : 'a Key.t -> 'a dyn
      | Both : 'a dyn * 'b dyn -> ('a * 'b) dyn

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
        | Both (x, y) -> X.pair (dyn store x) (dyn store y)
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
            let to_store = X.compose (X.pair (X.id ()) arr) to_store in
            let store =
              Store.map { f = (fun sarr -> X.compose sarr (X.fst ())) } store
            in
            let loc = Key.create () in
            let store = Store.add store loc (X.snd ()) in
            let s = State (to_store, store) in
            (s, Loc loc)
      in
      let loc = Key.create () in
      let to_store = X.id () in
      let store = Store.add Store.empty loc (X.id ()) in
      let s = State (to_store, store) in
      let State (to_store, store), d = static s (f (Loc loc)) in
      X.compose (dyn store d) to_store
  end

  module To_arrow_cartesian (X : S) = struct
    open X
    open Syntax (X)

    type 'a values =
      | Single : 'a Dyn.t -> 'a values
      | Pair : 'a values * 'b values -> ('a * 'b) values

    let rec value : type a. a values -> a Dyn.t = function
      | Single v -> v
      | Pair (vs1, vs2) -> Dyn.both (value vs1) (value vs2)

    type ('a, 'b) t = 'a values -> 'b values Static.t

    let id () v = return v
    let compose t1 t2 v = Static.bind (t2 v) t1

    let arr f vs =
      let+ v = Dyn.map f (value vs) in
      Single v

    let unit () _ = return (Single (Dyn.const ()))

    let fst (type a b) () : (a * b, a) t = function
      | Single v ->
          let+ v = Dyn.map fst v in
          Single v
      | Pair (vs1, _) -> return vs1

    let snd (type a b) () : (a * b, b) t = function
      | Single v ->
          let+ v = Dyn.map snd v in
          Single v
      | Pair (_, vs2) -> return vs2

    let pair t1 t2 vs =
      let* vs1 = t1 vs in
      let+ vs2 = t2 vs in
      Pair (vs1, vs2)

    let inject x vs =
      let+ v = x (value vs) in
      Single v

    let extract t v =
      let+ vs = t (Single v) in
      value vs
  end
end

module Classified = struct
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

  module Syntax (X : S) = struct
    let return = X.Static.return
    let ( let* ) = X.Static.bind
    let ( let+ ) m f = X.Static.bind m (fun x -> return (f x))
    let ( let& ) a f = X.Dyn.map f a
    let ( and& ) = X.Dyn.both
  end

  module Of_arrow_cartesian (X : Arrow_cartesian.S) = struct
    module S = Store.Make (X)

    type ('a, 'k) dyn =
      | Const : 'a -> ('a, 'k) dyn
      | Loc : ('a, 'k) Store.Key.t -> ('a, 'k) dyn
      | Both : ('a, 'k) dyn * ('b, 'k) dyn -> ('a * 'b, 'k) dyn

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
        | Both (x, y) -> X.pair (dyn store x) (dyn store y)
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
            let to_store = X.compose (X.pair (X.id ()) arr) to_store in
            let store =
              S.map { f = (fun sarr -> X.compose sarr (X.fst ())) } store
            in
            let loc = Inst.create () in
            let store = S.add store loc (X.snd ()) in
            let s = State (to_store, store) in
            (s, Loc loc)
      in
      let loc = Inst.create () in
      let to_store = X.id () in
      let store = S.add S.empty loc (X.id ()) in
      let s = State (to_store, store) in
      let State (to_store, store), d = static s (f.poly (Loc loc)) in
      X.compose (dyn store d) to_store
  end

  module To_arrow_cartesian (X : S) = struct
    open X
    open Syntax (X)

    type ('a, 'k) values =
      | Single : ('a, 'k) Dyn.t -> ('a, 'k) values
      | Pair : ('a, 'k) values * ('b, 'k) values -> ('a * 'b, 'k) values

    let rec value : type a. (a, _) values -> (a, _) Dyn.t = function
      | Single v -> v
      | Pair (vs1, vs2) -> Dyn.both (value vs1) (value vs2)

    type ('a, 'b) t = {
      kl : 'k. ('a, 'k) values -> (('b, 'k) values, 'k) Static.t;
    }

    let id () = { kl = (fun v -> return v) }

    let compose { kl = kl1 } { kl = kl2 } =
      { kl = (fun v -> Static.bind (kl2 v) kl1) }

    let arr f =
      {
        kl =
          (fun vs ->
            let+ v = Dyn.map f (value vs) in
            Single v);
      }

    let unit () = { kl = (fun _ -> return (Single (Dyn.const ()))) }

    let fst (type a b) () : (a * b, a) t =
      {
        kl =
          (function
          | Single v ->
              let+ v = Dyn.map fst v in
              Single v
          | Pair (vs1, _) -> return vs1);
      }

    let snd (type a b) () : (a * b, b) t =
      {
        kl =
          (function
          | Single v ->
              let+ v = Dyn.map snd v in
              Single v
          | Pair (_, vs2) -> return vs2);
      }

    let pair { kl = kl1 } { kl = kl2 } =
      {
        kl =
          (fun vs ->
            let* vs1 = kl1 vs in
            let+ vs2 = kl2 vs in
            Pair (vs1, vs2));
      }

    let inject x =
      {
        kl =
          (fun vs ->
            let+ v = x.poly (value vs) in
            Single v);
      }

    let extract { kl } =
      {
        poly =
          (fun v ->
            let+ vs = kl (Single v) in
            value vs);
      }
  end
end
