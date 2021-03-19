type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a t

let rev = List.rev
let rev_append = List.rev_append
