open! Import

module Span = Span_ns

type t = Span.t (* since wall-clock midnight *)
[@@deriving bin_io, compare, hash, typerep]


let start_of_day      : t = Span.zero
let start_of_next_day : t = Span.day

let approximate_end_of_day =
  Span.( - ) start_of_next_day Span.nanosecond

let to_span_since_start_of_day t = t

let [@inline never] input_out_of_bounds s =
  raise_s [%message "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
                      (s : Span.t)]
;;

let of_span_since_start_of_day_exn (s : Span.t) =
  (* Why we use [Span.(>)] rather than [.(>=)] below:

     We allow to represent the end-of-day sentinel value ([24.000000000h]), which is not
     itself a valid clock face time.  However, since valid clock face times readily
     round up to it, it's better to allow it to be represented. *)
  if Span.(<) s start_of_day || Span.(>) s start_of_next_day
  then input_out_of_bounds s
  else s

let add_exn t span = of_span_since_start_of_day_exn (Span.(+) t span)
let sub_exn t span = of_span_since_start_of_day_exn (Span.(-) t span)

let diff t u = Span.(-) t u

let create ?hr ?min ?sec ?ms ?us ?ns () =
  (* Similar to [Time.Ofday.create], if we detect a leap second we strip off all
     sub-second elements so that HH:MM:60.XXXXXXXXX is all mapped to HH:MM:60. *)
  let ms, us, ns =
    match sec with
    | Some 60 -> Some 0, Some 0, Some 0
    | _       -> ms,     us,     ns
  in
  of_span_since_start_of_day_exn (Span.create ?hr ?min ?sec ?ms ?us ?ns ())
;;

module Stable = struct
  module V1 = struct
    module T = struct
      type nonrec t = t [@@deriving compare, bin_io]

      let to_string_with_unit =
        let (/)   = Int63.(/)        in
        let (mod) = Int63.rem        in
        let (!)   = Int63.of_int     in
        let i     = Int63.to_int_exn in
        fun t ~unit ->
          if Span.(<) t start_of_day || Span.(<) start_of_next_day t
          then "Incorrect day"
          else begin
            let sixty    =   !60 in
            let thousand = !1000 in
            let ns = Span.to_int63_ns t in
            let us = ns / thousand      in let ns = ns mod thousand |> i in
            let ms = us / thousand      in let us = us mod thousand |> i in
            let  s = ms / thousand      in let ms = ms mod thousand |> i in
            let  m =  s /    sixty      in let  s =  s mod sixty    |> i in
            let  h =  m /    sixty |> i in let  m =  m mod sixty    |> i in
            let str =
              Bytes.create
                begin match unit with
                | `Millisecond -> 12
                | `Nanosecond  -> 18
                end
            in
            Digit_string_helpers.write_2_digit_int str ~pos:0 h;
            Bytes.set str 2 ':';
            Digit_string_helpers.write_2_digit_int str ~pos:3 m;
            Bytes.set str 5 ':';
            Digit_string_helpers.write_2_digit_int str ~pos:6 s;
            Bytes.set str 8 '.';
            Digit_string_helpers.write_3_digit_int str ~pos:9 ms;
            begin
              match unit with
              | `Millisecond -> ()
              | `Nanosecond  ->
                Digit_string_helpers.write_3_digit_int str ~pos:12 us;
                Digit_string_helpers.write_3_digit_int str ~pos:15 ns;
            end;
            Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str
          end

      let parse_nanoseconds string ~pos ~until =
        let open Int.O in
        let digits     = ref 0   in
        let num_digits = ref 0   in
        let pos        = ref pos in
        (* read up to 10 digits; store the first 9, use the 10th to round *)
        while !pos < until && !num_digits < 10 do
          let c = String.get string !pos in
          if Char.is_digit c
          then begin
            incr num_digits;
            if !num_digits < 10
            then digits := (!digits * 10) + Char.get_digit_exn c
            else if Char.get_digit_exn c >= 5
            then incr digits
            else ()
          end;
          incr pos
        done;
        (* if there are missing digits, add zeroes *)
        if !num_digits < 9
        then begin
          digits := !digits * Int.pow 10 (9 - !num_digits)
        end;
        !digits

      let of_string =
        let create_ofday string ~hr ~min ~sec ~subsec_pos ~subsec_len =
          let nanoseconds =
            if Int.equal subsec_len 0
            then 0
            else parse_nanoseconds string
                   ~pos:   (subsec_pos + 1)
                   ~until: (subsec_pos + subsec_len)
          in
          Span.of_int63_ns (Int63.of_int nanoseconds)
          |> Span.( + ) (Span.scale_int Span.second sec)
          |> Span.( + ) (Span.scale_int Span.minute min)
          |> Span.( + ) (Span.scale_int Span.hour   hr)
          |> of_span_since_start_of_day_exn
        in
        fun string ->
          Ofday_helpers.parse string ~f:create_ofday

      let t_of_sexp sexp : t =
        match sexp with
        | Sexp.List _ -> of_sexp_error "expected an atom" sexp
        | Sexp.Atom s -> (try of_string s with exn -> of_sexp_error_exn exn sexp)

      let to_string (t : t) =
        to_string_with_unit t ~unit:`Nanosecond

      let sexp_of_t (t : t) = Sexp.Atom (to_string t)

      let to_int63 t = Span_ns.Stable.V2.to_int63 t

      let of_int63_exn t =
        of_span_since_start_of_day_exn (Span_ns.Stable.V2.of_int63_exn t)
    end
    include T
    include Comparator.Stable.V1.Make (T)
  end
end

let sexp_of_t = Stable.V1.sexp_of_t
let t_of_sexp = Stable.V1.t_of_sexp
let of_string = Stable.V1.of_string
let to_string = Stable.V1.to_string

let to_millisecond_string t =
  Stable.V1.to_string_with_unit t ~unit:`Millisecond

include Identifiable.Make (struct
    type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
    let module_name = "Core.Time_ns.Ofday"
    let hash = Span.hash
    let of_string, to_string = of_string, to_string
  end)
include (Span : Comparisons.S with type t := t)
