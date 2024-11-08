Nonempty_set
=============

Roughly speaking, `Nonempty_set` is to `Set` as `Nonempty_list` is to `List`.

A `('a, 'cw) Nonempty_set.t` is a `('a, 'cw) Set.t` which is
guaranteed to not be empty.  This invariant allows us to provide some
additional type-safe operations, like [reduce], where the equivalent
operation over a `Set` would be potentially exn-raising.
