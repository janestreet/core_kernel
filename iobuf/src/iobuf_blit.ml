open! Core
include Iobuf_blit_intf.Definitions
open Iobuf_type

module Blit = struct
  module T_dst = struct
    include T_src

    type nonrec ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) Iobuf_type.t

    let create_bool = create
    let create_like ~len _ = create ~len

    let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
      (* Unlike other blitting functions, we use [Bigstring.unsafe_blit] here (regardless
         of the value of [unsafe_is_safe]), since we have two [Iobuf.t]s and can therefore
         bounds-check both buffers before calling [Bigstring.unsafe_blit]. *)
      Bigstring.unsafe_blit
        ~len
        ~src:([%template buf [@mode local]] src)
        ~src_pos:(unsafe_buf_pos src ~pos:src_pos ~len)
        ~dst:([%template buf [@mode local]] dst)
        ~dst_pos:(unsafe_buf_pos dst ~pos:dst_pos ~len) [@nontail]
    ;;
  end

  include
    Base_for_tests.Test_blit.Make1_phantom2_and_test [@modality portable]
      (struct
        include Char_elt

        type _ t = char
      end)
      (T_dst)

  (* Workaround the inability of the compiler to inline in the presence of functors. *)
  let unsafe_blit = T_dst.unsafe_blit

  let blit_maximal ~src ?(src_pos = 0) ~dst ?(dst_pos = 0) () =
    let len = min (length src - src_pos) (length dst - dst_pos) in
    blit ~src ~src_pos ~dst ~dst_pos ~len;
    len
  ;;
end

module Blit_consume = struct
  let unsafe_blit ~src ~dst ~dst_pos ~len =
    Blit.unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
    unsafe_advance src len
  ;;

  let blit ~src ~dst ~dst_pos ~len =
    Blit.blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
    unsafe_advance src len
  ;;

  let blito ~src ?(src_len = length src) ~dst ?(dst_pos = 0) () =
    blit ~src ~dst ~dst_pos ~len:src_len
  ;;

  let sub src ~len =
    let dst = Blit.sub src ~pos:0 ~len in
    unsafe_advance src len;
    dst
  ;;

  let subo ?len src =
    let len =
      match len with
      | None -> length src
      | Some len -> len
    in
    sub src ~len
  ;;

  let blit_maximal ~src ~dst ?(dst_pos = 0) () =
    let len = min (length src) (length dst - dst_pos) in
    blit ~src ~dst ~dst_pos ~len;
    len
  ;;
end

module Blit_fill = struct
  let unsafe_blit ~src ~src_pos ~dst ~len =
    Blit.unsafe_blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    unsafe_advance dst len
  ;;

  let blit ~src ~src_pos ~dst ~len =
    Blit.blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    unsafe_advance dst len
  ;;

  let blito ~src ?(src_pos = 0) ?(src_len = length src - src_pos) ~dst () =
    blit ~src ~src_pos ~dst ~len:src_len
  ;;

  let blit_maximal ~src ?(src_pos = 0) ~dst () =
    let len = min (length src - src_pos) (length dst) in
    blit ~src ~src_pos ~dst ~len;
    len
  ;;
end

module Blit_consume_and_fill = struct
  external phys_similar : ('a[@local_opt]) -> ('b[@local_opt]) -> bool @@ portable = "%eq"

  let unsafe_blit ~src ~dst ~len =
    if phys_similar src dst
    then advance src len
    else (
      Blit.unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
      unsafe_advance src len;
      unsafe_advance dst len)
  ;;

  let blit ~src ~dst ~len =
    if phys_similar src dst
    then advance src len
    else (
      Blit.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
      unsafe_advance src len;
      unsafe_advance dst len)
  ;;

  let blito ~src ?(src_len = length src) ~dst () = blit ~src ~dst ~len:src_len

  let blit_maximal ~src ~dst =
    let len = min (length src) (length dst) in
    (* [len] is naturally validated to be correct; don't double-check it. Sadly, we can't
       do this for the other [Blit_*] modules, as they can have invalid
       [src_pos]/[dst_pos] values which a) have to be checked on their own and b) can lead
       to the construction of unsafe [len] values. *)
    unsafe_blit ~src ~dst ~len;
    len
  ;;
end
