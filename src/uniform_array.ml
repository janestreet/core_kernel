module List = Core_list

(* WARNING:
   We use non-memory-safe things throughout the [Trusted] module.
   Most of it is only safe in combination with the type signature (e.g. exposing
   [val copy : 'a t -> 'b t] would be a big mistake). *)
module Trusted : sig

  type 'a t
  val empty : 'a t
  val unsafe_create_uninitialized : len:int -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val length : 'a t -> int
  val unsafe_blit : ('a t, 'a t) Blit_intf.blit
  val copy : 'a t -> 'a t

end = struct

  type 'a t = Obj_array.t

  let empty = Obj_array.empty

  let unsafe_create_uninitialized ~len = Obj_array.create ~len

  let get arr i = Obj.obj (Obj_array.get arr i)
  let set arr i x = Obj_array.set arr i (Obj.repr x)
  let unsafe_get arr i = Obj.obj (Obj_array.unsafe_get arr i)
  let unsafe_set arr i x = Obj_array.unsafe_set arr i (Obj.repr x)

  let length = Obj_array.length

  let unsafe_blit = Obj_array.unsafe_blit

  let copy = Obj_array.copy

end

include Trusted

let init l ~f =
  if l < 0 then invalid_arg "Uniform_array.init"
  else
    let res = unsafe_create_uninitialized ~len:l in
    for i = 0 to l - 1 do
      unsafe_set res i (f i)
    done;
    res

let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)

let map a ~f = init ~f:(fun i -> f (unsafe_get a i)) (length a)

let iter a ~f =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let to_list t = List.init ~f:(get t) (length t)

let of_list l =
  let len = List.length l in
  let res = unsafe_create_uninitialized ~len in
  List.iteri l ~f:(fun i x -> set res i x);
  res

let create ~len x =
  let res = unsafe_create_uninitialized ~len in
  for i = 0 to len-1
  do
    unsafe_set res i x
  done;
  res

(* It is not safe for [to_array] to be the identity function because we have code that
   relies on [float array]s being unboxed, for example in [bin_write_array]. *)
let to_array t = Core_array.init (length t) ~f:(fun i -> unsafe_get t i)

include Sexpable.Of_sexpable1(Core_array)(struct
    type nonrec 'a t = 'a t
    let to_sexpable = to_array
    let of_sexpable = of_array
  end)

include Binable.Of_binable1(Core_array)(struct
    type nonrec 'a t = 'a t
    let to_binable = to_array
    let of_binable = of_array
  end)

module Sequence = struct
  let length = length
  let get    = get
  let set    = set
end

include
  Blit.Make1
    (struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]
      type 'a z = 'a
      include Sequence
      let create_like ~len t =
        if len = 0
        then empty
        else (assert (length t > 0); create ~len (get t 0))
      ;;
      let unsafe_blit = unsafe_blit
      let create_bool ~len = create ~len false
    end)


let%bench_module "" = (
  module struct

    module Dummy = struct
      type t = {
        a : int;
        b : int }

      let dummy = { a = 0; b = 0 }
    end

    let len = 1000

    let array_get a init =
      let s = ref init in
      for i = 0 to len - 1 do
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
        s := Array.get a i;
      done;
      assert (!s = init)

    let boxed_array_get a init =
      let s0 = ref init in
      let s1 = ref init in
      let s2 = ref init in
      let s3 = ref init in
      let s4 = ref init in
      let s5 = ref init in
      let s6 = ref init in
      let s7 = ref init in
      let s8 = ref init in
      let s9 = ref init in
      for i = 0 to len - 1 do
        s0 := get a i;
        s1 := get a i;
        s2 := get a i;
        s3 := get a i;
        s4 := get a i;
        s5 := get a i;
        s6 := get a i;
        s7 := get a i;
        s8 := get a i;
        s9 := get a i;
      done;
      assert (!s0 = init);
      assert (!s1 = init);
      assert (!s2 = init);
      assert (!s3 = init);
      assert (!s4 = init);
      assert (!s5 = init);
      assert (!s6 = init);
      assert (!s7 = init);
      assert (!s8 = init);
      assert (!s9 = init)

    let array_unsafe_get a init =
      let s = ref init in
      for i = 0 to len - 1 do
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i;
        s := Array.unsafe_get a i
      done;
      assert (!s = init)

    let boxed_array_unsafe_get a init =
      let s0 = ref init in
      let s1 = ref init in
      let s2 = ref init in
      let s3 = ref init in
      let s4 = ref init in
      let s5 = ref init in
      let s6 = ref init in
      let s7 = ref init in
      let s8 = ref init in
      let s9 = ref init in
      for i = 0 to len - 1 do
        s0 := unsafe_get a i;
        s1 := unsafe_get a i;
        s2 := unsafe_get a i;
        s3 := unsafe_get a i;
        s4 := unsafe_get a i;
        s5 := unsafe_get a i;
        s6 := unsafe_get a i;
        s7 := unsafe_get a i;
        s8 := unsafe_get a i;
        s9 := unsafe_get a i
      done;
      assert (!s0 = init);
      assert (!s1 = init);
      assert (!s2 = init);
      assert (!s3 = init);
      assert (!s4 = init);
      assert (!s5 = init);
      assert (!s6 = init);
      assert (!s7 = init);
      assert (!s8 = init);
      assert (!s9 = init)

    let array_set a v1 v2 =
      for i = 0 to len - 1 do
        Array.set a i v1;
        Array.set a i v2;
        Array.set a i v1;
        Array.set a i v2;
        Array.set a i v1;
        Array.set a i v2;
        Array.set a i v1;
        Array.set a i v2;
        Array.set a i v1;
        Array.set a i v2
      done

    let boxed_array_set a v1 v2 =
      for i = 0 to len - 1 do
        set a i v1;
        set a i v2;
        set a i v1;
        set a i v2;
        set a i v1;
        set a i v2;
        set a i v1;
        set a i v2;
        set a i v1;
        set a i v2;
      done

    let array_unsafe_set a v1 v2 =
      for i = 0 to len - 1 do
        Array.unsafe_set a i v1;
        Array.unsafe_set a i v2;
        Array.unsafe_set a i v1;
        Array.unsafe_set a i v2;
        Array.unsafe_set a i v1;
        Array.unsafe_set a i v2;
        Array.unsafe_set a i v1;
        Array.unsafe_set a i v2;
        Array.unsafe_set a i v1;
        Array.unsafe_set a i v2;
      done

    let boxed_array_unsafe_set a v1 v2 =
      for i = 0 to len - 1 do
        unsafe_set a i v1;
        unsafe_set a i v2;
        unsafe_set a i v1;
        unsafe_set a i v2;
        unsafe_set a i v1;
        unsafe_set a i v2;
        unsafe_set a i v1;
        unsafe_set a i v2;
        unsafe_set a i v1;
        unsafe_set a i v2;
      done

    let dummy = Dummy.dummy
    let dummy2 = { Dummy.a = 1; b = 0 }

    module Array = Core_array
    let int_array   : int     array = Array.create ~len 0
    let int_t       : int     t     =       create ~len 0
    let float_array : float   array = Array.create ~len 0.
    let float_t     : float   t     =       create ~len 0.
    let dummy_array : Dummy.t array = Array.create ~len dummy
    let dummy_t     : Dummy.t t     =       create ~len dummy

    let%bench "        Array.get        int   x10" =       array_get        int_array   0
    let%bench "Uniform_array.get        int   x10" = boxed_array_get        int_t       0
    let%bench "        Array.get        float x10" =       array_get        float_array 0.
    let%bench "Uniform_array.get        float x10" = boxed_array_get        float_t     0.
    let%bench "        Array.get        dummy x10" =       array_get        dummy_array dummy
    let%bench "Uniform_array.get        dummy x10" = boxed_array_get        dummy_t     dummy
    let%bench "        Array.unsafe_get int   x10" =       array_unsafe_get int_array   0
    let%bench "Uniform_array.unsafe_get int   x10" = boxed_array_unsafe_get int_t       0
    let%bench "        Array.unsafe_get float x10" =       array_unsafe_get float_array 0.
    let%bench "Uniform_array.unsafe_get float x10" = boxed_array_unsafe_get float_t     0.
    let%bench "        Array.unsafe_get dummy x10" =       array_unsafe_get dummy_array dummy
    let%bench "Uniform_array.unsafe_get dummy x10" = boxed_array_unsafe_get dummy_t     dummy
    let%bench "        Array.set        int   x10" =       array_set        int_array   0 1
    let%bench "Uniform_array.set        int   x10" = boxed_array_set        int_t       0 1
    let%bench "        Array.set        float x10" =       array_set        float_array 0. 1.
    let%bench "Uniform_array.set        float x10" = boxed_array_set        float_t     0. 1.
    let%bench "        Array.set        dummy x10" =       array_set        dummy_array dummy dummy2
    let%bench "Uniform_array.set        dummy x10" = boxed_array_set        dummy_t     dummy dummy2
    let%bench "        Array.unsafe_set int   x10" =       array_unsafe_set int_array   0 1
    let%bench "Uniform_array.unsafe_set int   x10" = boxed_array_unsafe_set int_t       0 1
    let%bench "        Array.unsafe_set float x10" =       array_unsafe_set float_array 0. 1.
    let%bench "Uniform_array.unsafe_set float x10" = boxed_array_unsafe_set float_t     0. 1.
    let%bench "        Array.unsafe_set dummy x10" =       array_unsafe_set dummy_array dummy dummy2
    let%bench "Uniform_array.unsafe_set dummy x10" = boxed_array_unsafe_set dummy_t     dummy dummy2
  end)
