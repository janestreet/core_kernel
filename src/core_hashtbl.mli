(** Core_hashtbl is a reimplementation of the standard MoreLabels.Hashtbl.  Its worst case
    time complexity is O(log(N)) for lookups and additions, unlike the standard
    MoreLabels.Hashtbl, which is O(N)

    A hash table is implemented as an array of AVL trees (see [Avltree]). If
    [growth_allowed] (default true) is false then [size] is the final size of the array,
    the table can always hold more elements than [size], however they will all go into
    tree nodes. If it is true (default) then the array will double in size when the number
    of elements in the table reaches twice the size of the array. When this happens all
    existing elements will be reinserted, which can take a long time. If you care about
    latency set [size] and [growth_allowed=false] if possible.

    In most cases, functions passed as arguments to hash table accessors must not mutate
    the hash table while it is being accessed, as it will result an an exception.  For
    example, [iter] and [change] take a function [f] which must not modify [t].  In a few
    cases, mutation is allowed, such as in [Hashtbl.find_and_call], where all access to
    [t] is finished before the [~if_found] and [~if_not_found] arguments are invoked.

    We have three kinds of hash table modules:

    Hashtbl
    Hashtbl.Poly
    Key.Table       (a class of similar modules)

    There are three kinds of hash-table functions:

    creation from nothing (create, of_alist)
    sexp converters (t_of_sexp, sexp_of_t, and bin_io too)
    accessors and mappers (fold, mem, find, map, filter_map, ...)

    Here is a table showing what classes of functions are available in each kind
    of hash-table module:

                   creation   sexp-conv   accessors
    Hashtbl                                   X
    Hashtbl.Poly      X           X
    Key.Table         X           X           X'

    The entry marked with X' is there for historical reasons, and may be eliminated at
    some point.  The upshot is that one should use [Hashtbl] for accessors, [Hashtbl.Poly]
    for hash-table creation and sexp conversion using polymorphic compare/hash, and
    [Key.Table] for hash-table creation and sexp conversion using [Key.compare] and
    [Key.hash].
 *)

(** For many students of ocaml, using hashtables is complicated by the
    functors.  Here are a few tips: *)

(** For a list of hashtable functions see [Hashtbl_intf.S].*)

(** To create a hashtable with string keys use String.Table.
   {[
    let table = String.Table.create () ~size:4 in
    List.iter ~f:(fun (key, data) -> Hashtbl.set table ~key ~data)
      [ ("A", 1); ("B", 2); ("C", 3); ];
    Hashtbl.find table "C" ]}
    Here 4 need only be a guess at the hashtable's future size.
    There are other similar pre-made hashtables, eg
      Int63.Table or Host_and_port.Table. *)


  (** To create a hashtable with a custom key type use Hashable.
      {[
        module Key = struct
          module T = struct
            type t = String.t * Int63.t [@@deriving sexp]
            let compare = compare
            let hash = Hashtbl.hash
          end
          include T
          include Hashable.Make (T)
        end
        let table = Key.Table.create () ~size:4 in
        List.iter ~f:(fun (key, data) -> Hashtbl.set table ~key ~data)
          [ (("pi", Int63.zero), 3.14159);
            (("e", Int63.minus_one), 2.71828);
            (("Euler", Int63.one), 0.577215);
          ];
        Hashtbl.find table ("pi", Int63.zero)]}
      Performance {i may} improve if you define [equal] and [hash] explicitly, eg:
      {[
        let equal (x, y) (x', y') = String.(=) x x' && Int63.(=) y y'
        let hash (x, y) = String.hash x + Int63.hash y * 65599 ]} *)

include Core_hashtbl_intf.Hashtbl
