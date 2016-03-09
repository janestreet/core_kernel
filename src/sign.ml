include Sign0

include Identifiable.Make(T)

let to_float = function
  | Neg -> -1.
  | Zero -> 0.
  | Pos -> 1.

let flip = function
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg

let ( * ) t t' = of_int (to_int t * to_int t')

let%test _ = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero

let ( < ) = Pervasives.(<)
let ( = ) = Pervasives.(=)
let%test _ = compare Neg Zero < 0 && compare Zero Pos < 0
let%test _ = List.for_all (fun t -> t = (t |> to_int   |> of_int  )) all
let%test _ = List.for_all (fun i -> i = (i |> of_int   |> to_int  )) [ -1; 0; 1 ]
