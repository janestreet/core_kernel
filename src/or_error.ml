open! Import

type 'a t = ('a, Error.t) Result.t [@@deriving bin_io]

module T :
  module type of struct
  include Base.Or_error
end
with type 'a t := 'a t =
  Base.Or_error

include T

module Expect_test_config = struct
  module IO = struct
    type nonrec 'a t = 'a t

    include T
  end

  open T

  let flush () = return ()
  let run f = ok_exn (f ())
  let flushed () = true
  let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
end

module Stable = struct
  module V1 = struct
    type 'a t = ('a, Error.Stable.V1.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end

  module V2 = struct
    type 'a t = ('a, Error.Stable.V2.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end
end
