open! Import
type t = bytes

let create = Caml.Bytes.create
let length = Caml.Bytes.length
