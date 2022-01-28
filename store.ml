module Eq = struct
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Data = struct
  module type S = sig
    type ('a, 'b) t
  end
end

module Key = struct
  type ('a, 'k) repr = ..

  module type S = sig
    type index
    type k
    type ('a, 'k) repr += C : (index, k) repr
  end

  type ('a, 'k) t = (module S with type index = 'a and type k = 'k)

  let equal (type a b k) ((module A) : (a, k) t) ((module B) : (b, k) t) :
      (a, b) Eq.t option =
    match A.C with B.C -> Some Eq.Refl | _ -> None

  module Instance () = struct
    type k
    type nonrec 'a t = ('a, k) t

    let create (type a) () : a t =
      (module struct
        type index = a
        type nonrec k = k
        type ('a, 'k) repr += C : (index, k) repr
      end)
  end

  let id (type a k) ((module M) : (a, k) t) = [%extension_constructor M.C]

  module Packed = struct
    type ('a, 'k) unpacked = ('a, 'k) t
    type t = Packed : ('a, 'k) unpacked -> t [@@unboxed]

    let id (Packed t) = id t
    let compare t1 t2 = compare (id t1) (id t2)
  end
end

module Map = Map.Make (Key.Packed)

module Make (Data : Data.S) = struct
  module Sigma = struct
    type ('a, 'k) t =
      | Sigma : { key : ('b, 'k) Key.t; data : ('a, 'b) Data.t } -> ('a, 'k) t
  end

  type ('a, 'k) t = ('a, 'k) Sigma.t Map.t

  let empty = Map.empty

  let add t key data =
    let packed = Key.Packed.Packed key in
    let sigma = Sigma.Sigma { key; data } in
    Map.add packed sigma t

  let find (type a b k) (t : (a, k) t) (key : (b, k) Key.t) :
      (a, b) Data.t option =
    let packed = Key.Packed.Packed key in
    match Map.find_opt packed t with
    | None -> None
    | Some (Sigma.Sigma { key = key'; data }) -> (
        match Key.equal key key' with
        | None -> assert false
        | Some Refl -> Some data)

  type ('a, 'b) map = { f : 'c. ('a, 'c) Data.t -> ('b, 'c) Data.t } [@@unboxed]

  let map { f } t =
    Map.map
      (fun (Sigma.Sigma { key; data }) -> Sigma.Sigma { key; data = f data })
      t
end
