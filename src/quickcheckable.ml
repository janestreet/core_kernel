module type S = Quickcheck_intf.S
  with type 'a gen := 'a Quickcheck.gen
  with type 'a obs := 'a Quickcheck.obs
  with type 'a shr := 'a Quickcheck.shr

module type S1 = Quickcheck_intf.S1
  with type 'a gen := 'a Quickcheck.gen
  with type 'a obs := 'a Quickcheck.obs
  with type 'a shr := 'a Quickcheck.shr

module type S2 = Quickcheck_intf.S2
  with type 'a gen := 'a Quickcheck.gen
  with type 'a obs := 'a Quickcheck.obs
  with type 'a shr := 'a Quickcheck.shr

module type S_bounded = Quickcheck_intf.S_bounded
  with type 'a gen := 'a Quickcheck.gen
  with type 'a obs := 'a Quickcheck.obs
  with type 'a shr := 'a Quickcheck.shr
