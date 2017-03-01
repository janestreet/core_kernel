open! Import

module Binable = Binable0
module List = Base.List
module Queue = Base.Linked_queue

include Queue

include
  Bin_prot.Utils.Make_iterable_binable1 (struct

    type 'a t = 'a Queue.t
    type 'a el = 'a [@@deriving bin_io]

    let caller_identity = Bin_prot.Shape.Uuid.of_string "800df9a0-4992-11e6-881d-ffe1a5c8aced"

    let module_name = Some "Core_kernel.Linked_queue"

    let length = length

    let iter = iter

    (* Bin_prot reads the elements in the same order they were written out, as determined
       by [iter].  So, we can ignore the index and just enqueue each element as it is read
       in. *)
    let init ~len ~next =
      let t = create () in
      for _ = 1 to len do
        enqueue t (next ())
      done;
      t
  end)

let%test_module _ = (module struct
                      let m =
                        let module M  = struct
                          type 'a u = 'a t [@@deriving bin_io]
                          type t = int u [@@deriving bin_io]
                        end
                        in
                        (module M : Binable.S with type t = M.t)
                      ;;

                      let test list =
                        let t = of_list list in
                        let bigstring = Binable.to_bigstring m t in
                        let list' = to_list (Binable.of_bigstring m bigstring) in
                        [%compare.equal: int list] list list'
                      ;;

                      let%test _ = test []
                      let%test _ = test [ 1 ]
                      let%test _ = test [ 1; 2; 3 ]
                      let%test _ = test (List.init 10_000 ~f:Fn.id)

                    end)

