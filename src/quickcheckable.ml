open! Import

module type S = Quickcheck_intf.S
  with type 'a gen := 'a Quickcheck.Generator.t
  with type 'a obs := 'a Quickcheck.Observer.t
  with type 'a shr := 'a Quickcheck.Shrinker.t

module type S1 = Quickcheck_intf.S1
  with type 'a gen := 'a Quickcheck.Generator.t
  with type 'a obs := 'a Quickcheck.Observer.t
  with type 'a shr := 'a Quickcheck.Shrinker.t

module type S2 = Quickcheck_intf.S2
  with type 'a gen := 'a Quickcheck.Generator.t
  with type 'a obs := 'a Quickcheck.Observer.t
  with type 'a shr := 'a Quickcheck.Shrinker.t

module type S_int = Quickcheck_intf.S_int
  with type 'a gen := 'a Quickcheck.Generator.t
  with type 'a obs := 'a Quickcheck.Observer.t
  with type 'a shr := 'a Quickcheck.Shrinker.t
