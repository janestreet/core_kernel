(* C stub for int popcount to use the POPCNT instruction where possible *)
external int_popcount : int -> int = "int_math_int_popcount" "noalloc"

(* To maintain javascript compatibility and enable unboxing, we implement popcount in
   OCaml rather than use C stubs. Implementation adapted from:
   https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation *)
let int64_popcount =
  let open Int64 in
  let ( + ) = add in
  let ( - ) = sub in
  let ( * ) = mul in
  let ( lsr ) = shift_right_logical in
  let ( land ) = logand in
  let m1  = 0x5555555555555555L in (* 0b01010101... *)
  let m2  = 0x3333333333333333L in (* 0b00110011... *)
  let m4  = 0x0f0f0f0f0f0f0f0fL in (* 0b00001111... *)
  let h01 = 0x0101010101010101L in (* 1 bit set per byte *)
  (fun x ->
     (* gather the bit count for every pair of bits *)
     let x = x - ((x lsr 1) land m1) in
     (* gather the bit count for every 4 bits *)
     let x = (x land m2) + ((x lsr 2) land m2) in
     (* gather the bit count for every byte *)
     let x = (x + (x lsr 4)) land m4 in
     (* sum the bit counts in the top byte and shift it down *)
     to_int ((x * h01) lsr 56)) [@inline]

let int32_popcount =
  (* On 64-bit systems, this is faster than implementing using [int32] arithmetic. *)
  let mask = 0xffff_ffffL in
  (fun x -> int64_popcount (Int64.logand (Int64.of_int32 x) mask)) [@inline]

let nativeint_popcount =
  match Nativeint.size with
  | 32 -> (fun x -> int32_popcount (Nativeint.to_int32 x)) [@inline]
  | 64 -> (fun x -> int64_popcount (Int64.of_nativeint x)) [@inline]
  | _  -> assert false

(* Using [%bench_fun] to bind the input outside the benchmarked code actually has less
   overhead then using [%bench] naively. *)
let%bench_fun "popcount_bench_overhead" = let n = 0  in fun () -> Fn.id              n
let%bench_fun "int_popcount"            = let n = 0  in fun () -> int_popcount       n
let%bench_fun "int32_popcount"          = let n = 0l in fun () -> int32_popcount     n
let%bench_fun "int64_popcount"          = let n = 0L in fun () -> int64_popcount     n
let%bench_fun "nativeint_popcount"      = let n = 0n in fun () -> nativeint_popcount n
