open! Import

include Make_substring.F (struct
    type t = bytes

    let create = Bytes.create
    let length = Bytes.length
    let get    = Bytes.get
    module Blit = Make_substring.Blit
    let blit                = Blit.bytes_bytes
    let blit_to_string      = Blit.bytes_bytes
    let blit_to_bytes       = Blit.bytes_bytes
    let blit_to_bigstring   = Blit.bytes_bigstring
    let blit_from_string    = Blit.string_string
    let blit_from_bigstring = Blit.bigstring_string
    let of_bigstring bs = Bigstring.to_bytes bs
    let of_string s = Bytes.of_string s
  end)
