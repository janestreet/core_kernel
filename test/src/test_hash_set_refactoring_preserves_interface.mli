type 'a t = 'a Core_kernel.Hash_set.t

val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
val create : ?growth_allowed:bool -> ?size:int -> 'a Base.Hashtbl.Key.t -> 'a t

val of_list
  :  ?growth_allowed:bool
  -> ?size:int
  -> 'a Base.Hashtbl.Key.t
  -> 'a list
  -> 'a t

val length : 'a t -> int
val is_empty : 'a t -> bool
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

val fold_result
  :  'a t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum, 'e) result)
  -> ('accum, 'e) result

val fold_until
  :  'a t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum, 'final) Core_kernel.Continue_or_stop.t)
  -> finish:('accum -> 'final)
  -> 'final

val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val count : 'a t -> f:('a -> bool) -> int

val sum
  :  (module Base__.Container_intf.Summable with type t = 'sum)
  -> 'a t
  -> f:('a -> 'sum)
  -> 'sum

val find : 'a t -> f:('a -> bool) -> 'a option
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val mem : 'a t -> 'a -> bool
val copy : 'a t -> 'a t
val add : 'a t -> 'a -> unit
val strict_add : 'a t -> 'a -> unit Base__.Or_error.t
val strict_add_exn : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val strict_remove : 'a t -> 'a -> unit Base__.Or_error.t
val strict_remove_exn : 'a t -> 'a -> unit
val clear : 'a t -> unit
val equal : 'a t -> 'a t -> bool
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_inplace : 'a t -> f:('a -> bool) -> unit
val inter : 'key t -> 'key t -> 'key t
val union : 'a t -> 'a t -> 'a t
val diff : 'a t -> 'a t -> 'a t
val of_hashtbl_keys : ('a, 'b) Core_kernel.Hashtbl.t -> 'a t
val to_hashtbl : 'key t -> f:('key -> 'data) -> ('key, 'data) Core_kernel.Hashtbl.t
val hashable : 'key t -> 'key Base__Hashable_intf.Hashable.t

module type Elt_plain = Core_kernel__.Hashtbl.Key_plain
module type Elt = Core_kernel__.Hashtbl.Key
module type Elt_binable = Core_kernel__.Hashtbl.Key_binable

module type S_plain = sig
  type elt
  type t = elt Core_kernel.Hash_set.t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create : ('a, unit -> t) Base.Hash_set.create_options_without_first_class_module

  val of_list
    : ('a, elt list -> t) Base.Hash_set.create_options_without_first_class_module

  module Provide_of_sexp : functor
    (X : sig
       val t_of_sexp : Sexplib0.Sexp.t -> elt
     end)
    -> sig
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end

  module Provide_bin_io : functor
    (X : sig
       val bin_size_t : elt Bin_prot.Size.sizer
       val bin_write_t : elt Bin_prot.Write.writer
       val bin_read_t : elt Bin_prot.Read.reader
       val __bin_read_t__ : (int -> elt) Bin_prot.Read.reader
       val bin_shape_t : Bin_shape_lib.Bin_shape.t
       val bin_writer_t : elt Bin_prot.Type_class.writer0
       val bin_reader_t : elt Bin_prot.Type_class.reader0
       val bin_t : elt Bin_prot.Type_class.t0
     end)
    -> sig
      val bin_size_t : t Bin_prot.Size.sizer
      val bin_write_t : t Bin_prot.Write.writer
      val bin_read_t : t Bin_prot.Read.reader
      val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
      val bin_shape_t : Bin_shape_lib.Bin_shape.t
      val bin_writer_t : t Bin_prot.Type_class.writer0
      val bin_reader_t : t Bin_prot.Type_class.reader0
      val bin_t : t Bin_prot.Type_class.t0
    end
end

module type S = sig
  type elt
  type t = elt Core_kernel.Hash_set.t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create : ('a, unit -> t) Base.Hash_set.create_options_without_first_class_module

  val of_list
    : ('a, elt list -> t) Base.Hash_set.create_options_without_first_class_module

  module Provide_of_sexp : functor
    (X : sig
       val t_of_sexp : Sexplib0.Sexp.t -> elt
     end)
    -> sig
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end

  module Provide_bin_io : functor
    (X : sig
       val bin_size_t : elt Bin_prot.Size.sizer
       val bin_write_t : elt Bin_prot.Write.writer
       val bin_read_t : elt Bin_prot.Read.reader
       val __bin_read_t__ : (int -> elt) Bin_prot.Read.reader
       val bin_shape_t : Bin_shape_lib.Bin_shape.t
       val bin_writer_t : elt Bin_prot.Type_class.writer0
       val bin_reader_t : elt Bin_prot.Type_class.reader0
       val bin_t : elt Bin_prot.Type_class.t0
     end)
    -> sig
      val bin_size_t : t Bin_prot.Size.sizer
      val bin_write_t : t Bin_prot.Write.writer
      val bin_read_t : t Bin_prot.Read.reader
      val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
      val bin_shape_t : Bin_shape_lib.Bin_shape.t
      val bin_writer_t : t Bin_prot.Type_class.writer0
      val bin_reader_t : t Bin_prot.Type_class.reader0
      val bin_t : t Bin_prot.Type_class.t0
    end

  val t_of_sexp : Sexplib0.Sexp.t -> t
end

module type S_binable = sig
  type elt
  type t = elt Core_kernel.Hash_set.t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create : ('a, unit -> t) Base.Hash_set.create_options_without_first_class_module

  val of_list
    : ('a, elt list -> t) Base.Hash_set.create_options_without_first_class_module

  module Provide_of_sexp : functor
    (X : sig
       val t_of_sexp : Sexplib0.Sexp.t -> elt
     end)
    -> sig
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end

  module Provide_bin_io : functor
    (X : sig
       val bin_size_t : elt Bin_prot.Size.sizer
       val bin_write_t : elt Bin_prot.Write.writer
       val bin_read_t : elt Bin_prot.Read.reader
       val __bin_read_t__ : (int -> elt) Bin_prot.Read.reader
       val bin_shape_t : Bin_shape_lib.Bin_shape.t
       val bin_writer_t : elt Bin_prot.Type_class.writer0
       val bin_reader_t : elt Bin_prot.Type_class.reader0
       val bin_t : elt Bin_prot.Type_class.t0
     end)
    -> sig
      val bin_size_t : t Bin_prot.Size.sizer
      val bin_write_t : t Bin_prot.Write.writer
      val bin_read_t : t Bin_prot.Read.reader
      val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
      val bin_shape_t : Bin_shape_lib.Bin_shape.t
      val bin_writer_t : t Bin_prot.Type_class.writer0
      val bin_reader_t : t Bin_prot.Type_class.reader0
      val bin_t : t Bin_prot.Type_class.t0
    end

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val bin_size_t : t Bin_prot.Size.sizer
  val bin_write_t : t Bin_prot.Write.writer
  val bin_read_t : t Bin_prot.Read.reader
  val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
  val bin_shape_t : Bin_shape_lib.Bin_shape.t
  val bin_writer_t : t Bin_prot.Type_class.writer0
  val bin_reader_t : t Bin_prot.Type_class.reader0
  val bin_t : t Bin_prot.Type_class.t0
end

module Using_hashable = Core_kernel.Hash_set.Using_hashable
module Poly = Core_kernel.Hash_set.Poly
module Make_plain = Core_kernel.Hash_set.Make_plain
module Make = Core_kernel.Hash_set.Make
module Make_binable = Core_kernel.Hash_set.Make_binable
module Make_plain_with_hashable = Core_kernel.Hash_set.Make_plain_with_hashable
module Make_with_hashable = Core_kernel.Hash_set.Make_with_hashable
module Make_binable_with_hashable = Core_kernel.Hash_set.Make_binable_with_hashable

module type M_of_sexp = Base__.Hash_set_intf.M_of_sexp
module type Sexp_of_m = Base__.Hash_set_intf.Sexp_of_m

module M = Core_kernel.Hash_set.M

val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> 'elt t -> Sexplib0.Sexp.t
val m__t_of_sexp : (module M_of_sexp with type t = 'elt) -> Sexplib0.Sexp.t -> 'elt t
val m__t_sexp_grammar : Sexplib0.Private.Raw_grammar.t

module type M_quickcheck = Core_kernel__.Hash_set_intf.M_quickcheck

val quickcheck_generator_m__t
  :  (module M_quickcheck with type t = 'key)
  -> 'key t Base_quickcheck.Generator.t

val quickcheck_observer_m__t
  :  (module M_quickcheck with type t = 'key)
  -> 'key t Base_quickcheck.Observer.t

val quickcheck_shrinker_m__t
  :  (module M_quickcheck with type t = 'key)
  -> 'key t Base_quickcheck.Shrinker.t
