(* Functions for parsing time zone database files (zic files).

   A time zone file consists (conceptually - the representation is more
   compact) of an ordered list of (Time.t * [local_time_type]) that mark
   the boundaries (marked from the epoch) at which various time adjustment
   regimes are in effect.  This can also be thought of as breaking down all
   time past the epoch into ranges with a [local_time_type] that describes the
   offset from GMT to apply to each range to get local time.
*)

open Import
open Std_internal

include Zone_intf
exception Invalid_file_format of string [@@deriving sexp]

module Make (Time0 : Time0_intf.S) = struct
  let likely_machine_zones = ref [
    "America/New_York";
    "Europe/London";
    "Asia/Hong_Kong";
    "America/Chicago"
  ]

  module Stable = struct
    module V1 = struct
      module Regime = struct
        type t = {
          utc_off : Time0.Span.t;
          is_dst  : bool;
          abbrv   : string;
        }
      end

      (* holds information about when leap seconds should be applied - unused
         because we are translating based on a epoch system clock (see the Core_zone
         documentation). *)
      module Leap_second = struct
        type t = {
          time    : Time0.t;
          seconds : int;
        }
      end

      module Transition = struct
        type t = {
          start_time : Time0.t;
          new_regime : Regime.t
        }
      end

      (* IF THIS REPRESENTATION EVER CHANGES (particularly [name]), ENSURE THAT EITHER
         (1) all values serialize the same way in both representations, or
         (2) you add a new Time_internal.Zone version to stable.ml

         Note that serialization is basically exclusively via the [name] field,
         and we do not ultimately export the [sexp_of_t] that [with sexp_of]
         generates. *)
      type t = {
        name                      : string;
        original_filename         : string option;
        digest                    : Digest.t option;
        transitions               : Transition.t array;
        (* caches the index of the last transition we used to make lookups faster *)
        mutable last_regime_index : int;
        default_local_time_type   : Regime.t;
        leap_seconds              : Leap_second.t list;
      }

      (* this relies on zones with the same name having the same transitions *)
      let compare t1 t2 = String.compare t1.name t2.name

      let sexp_of_t t = Sexp.Atom t.name

      let original_filename zone = zone.original_filename
      let digest zone = zone.digest

      module Zone_file : sig
        val input_tz_file : zonename:string -> filename:string -> t
      end = struct
        let bool_of_int i = i <> 0

        let input_long_as_int32 =
          let long = String.create 4 in
          let int32_of_char chr = Int32.of_int_exn (int_of_char chr) in
          fun ic ->
            In_channel.really_input_exn ic ~buf:long ~pos:0 ~len:4;
            let sb1 = Int32.shift_left (int32_of_char long.[0]) 24 in
            let sb2 = Int32.shift_left (int32_of_char long.[1]) 16 in
            let sb3 = Int32.shift_left (int32_of_char long.[2]) 8 in
            let sb4 = int32_of_char long.[3] in
            Int32.bit_or (Int32.bit_or sb1 sb2) (Int32.bit_or sb3 sb4)
        ;;

        (* Note that this is only safe to use on numbers that will fit into a 31-bit
           int. UNIX timestamps won't, for example.  In our case this is only used
           to hold small numbers that are never interpreted as timestamps. *)
        let input_long_as_int ic = Int32.to_int_exn (input_long_as_int32 ic)

        let input_long_as_span ic =
          input_long_as_int32 ic
          |> Time0.Span.of_int32_seconds
        ;;

        let input_long_as_time ic =
          input_long_as_span ic
          |> Time0.of_span_since_epoch
        ;;

        let input_long_long_as_time ic =
          let int63_of_char chr = Int63.of_int_exn (int_of_char chr) in
          let shift c bits = Int63.shift_left (int63_of_char c) bits in
          let long_long = String.create 8 in
          In_channel.really_input_exn ic ~buf:long_long ~pos:0 ~len:8;
          let result =                      shift long_long.[0] 56 in
          let result = Int63.bit_or result (shift long_long.[1] 48) in
          let result = Int63.bit_or result (shift long_long.[2] 40) in
          let result = Int63.bit_or result (shift long_long.[3] 32) in
          let result = Int63.bit_or result (shift long_long.[4] 24) in
          let result = Int63.bit_or result (shift long_long.[5] 16) in
          let result = Int63.bit_or result (shift long_long.[6] 8) in
          let result = Int63.bit_or result (int63_of_char long_long.[7]) in
          Time0.of_span_since_epoch (Time0.Span.of_int63_seconds result)
        ;;

        let input_list ic ~len ~f =
          let rec loop c lst =
            if c > 0 then loop (c - 1) ((f ic) :: lst)
            else List.rev lst
          in
          loop len []
        ;;

        let input_array ic ~len ~f = Array.of_list (input_list ic ~len ~f)

        let input_regime ic =
          let utc_off = input_long_as_span ic in
          let is_dst = bool_of_int (Option.value_exn (In_channel.input_byte ic)) in
          let abbrv_index = Option.value_exn (In_channel.input_byte ic) in
          let lt abbrv =
            { Regime.
              utc_off;
              is_dst  = is_dst;
              abbrv   = abbrv;
            }
          in
          (lt,abbrv_index)
        ;;

        let input_abbreviations ic ~len =
          let raw_abbrvs =
            input_list ic ~len ~f:(fun ic -> Option.value_exn (In_channel.input_char ic))
          in
          let buf = Buffer.create len in
          let _,indexed_abbrvs =
            List.fold raw_abbrvs ~init:(0, Map.Poly.empty)
              ~f:(fun (index,abbrvs) c ->
                match c with
                | '\000' ->
                  let data = Buffer.contents buf in
                  let next_index = index + (String.length data) + 1 in
                  let abbrvs = Map.add abbrvs ~key:index ~data in
                  Buffer.clear buf;
                  (next_index,abbrvs)
                | c -> Buffer.add_char buf c; (index,abbrvs))
          in
          if Buffer.length buf <> 0 then
            raise
              (Invalid_file_format "missing \000 terminating character in input_abbreviations");
          indexed_abbrvs
        ;;

        let input_tz_file_gen ~input_transition ~input_leap_second ic =
          let utc_local_count    = input_long_as_int ic in
          let std_wall_count     = input_long_as_int ic in
          let leap_count         = input_long_as_int ic in
          let transition_count   = input_long_as_int ic in
          let type_count         = input_long_as_int ic in
          let abbrv_char_count   = input_long_as_int ic in
          let transition_times   = input_list ic ~f:input_transition ~len:transition_count in
          let transition_indices =
            input_list ic ~f:(fun ic ->
              Option.value_exn (In_channel.input_byte ic)) ~len:transition_count
          in
          let regimes            = input_list ic ~f:input_regime ~len:type_count in
          let abbreviations      = input_abbreviations ic ~len:abbrv_char_count in
          let leap_seconds       = input_list ic ~f:input_leap_second ~len:leap_count in
          (* The following two arrays indicate two boolean values per regime that
             represent a three-value type that would translate to:

             type transition_type = UTC | Standard | Wall_clock

             However, these are only used by the system library when handling the case where the
             TZ variable is set, not to a time zone name, but instead is of the form:

             TZ = "std offset dst offset, rule"

             Which is deeply obscure, and almost certainly a mistake to use.  This library makes
             no pretense about handling this case.  We continue to read them in for
             completeness, and because it's possible that we will later discover a case where
             they are used. *)
          let _std_wall_indicators =
            input_array ic ~len:std_wall_count
              ~f:(fun ic -> bool_of_int (Option.value_exn (In_channel.input_byte ic)))
          in
          let _utc_local_indicators =
            input_array ic ~len:utc_local_count
              ~f:(fun ic -> bool_of_int (Option.value_exn (In_channel.input_byte ic)))
          in
          let regimes =
            Array.of_list (List.map regimes
                             ~f:(fun (lt,abbrv_index) ->
                               let abbrv = Map.find_exn abbreviations abbrv_index in
                               lt abbrv
                             ))
          in
          let raw_transitions =
            List.map2_exn transition_times transition_indices
              ~f:(fun time index ->
                let regime = regimes.(index) in
                (time, regime))
          in
          let transitions =
            let rec make_transitions acc l =
              match l with
              | [] -> Array.of_list (List.rev acc)
              | (start_time,regime) :: rest ->
                make_transitions
                  ({Transition.
                     start_time = start_time;
                     new_regime = regime
                   } :: acc) rest
            in
            make_transitions [] raw_transitions
          in
          let default_local_time_type =
            match
              Array.find regimes ~f:(fun r -> not r.Regime.is_dst)
            with
            | None -> regimes.(0)
            | Some ltt -> ltt
          in
          (fun name ~original_filename ~digest ->
             { name                    = name
             ; original_filename       = Some original_filename
             ; digest                  = Some digest
             ; transitions             = transitions
             ; last_regime_index       = 0
             ; default_local_time_type = default_local_time_type
             ; leap_seconds            = leap_seconds
             }
          )
        ;;

        let input_leap_second_gen ~input_leap_second ic =
          let leap_time = input_leap_second ic in
          let seconds   = input_long_as_int ic in
          { Leap_second.
            time    = leap_time;
            seconds = seconds;
          }
        ;;

        let read_header ic =
          let buf = String.create 4 in
          In_channel.really_input_exn ic ~buf ~pos:0 ~len:4;
          if buf <> "TZif" then
            raise (Invalid_file_format "magic characters TZif not present");
          let version =
            match In_channel.input_char ic with
            | Some '\000' -> `V1
            | Some '2'    -> `V2
            | None        ->
              raise (Invalid_file_format "expected version, found nothing")
            | Some bad_version ->
              raise (Invalid_file_format (sprintf "version (%c) is invalid" bad_version))
          in
          (* space reserved for future use in the format *)
          In_channel.really_input_exn ic ~buf:(String.create 15) ~pos:0 ~len:15;
          version
        ;;

        let input_tz_file_v1 ic =
          let input_leap_second =
            input_leap_second_gen ~input_leap_second:input_long_as_time
          in

          input_tz_file_gen ~input_transition:input_long_as_time
            ~input_leap_second ic
        ;;

      (*
           version 2 timezone files have the format:

           part 1 - exactly the same as v1

           part 2 - same format as v1, except that 8 bytes are used to store
           transition times and leap seconds

           part 3 - a newline-encloded, POSIX-TZ-environment-variable-style
           string for use in handling instants after the last transition time
           stored in the file (with nothing between the newlines if there is no
           POSIX representation for such instants)

           We handle files in this format by parsing the first part exactly as a v1 timezone
           file and then continuing to parse with 64bit reading functions in the right places.
        *)
        let input_tz_file_v2 ic =
          let _ = input_tz_file_v1 ic in
          (* the header is fully repeated *)
          assert (read_header ic = `V2);
          let input_leap_second =
            input_leap_second_gen ~input_leap_second:input_long_long_as_time
          in
          input_tz_file_gen ~input_transition:input_long_long_as_time
            ~input_leap_second ic
        ;;

        let input_tz_file ~zonename ~filename =
          try
            protectx (In_channel.create filename) ~finally:In_channel.close ~f:(fun ic ->
              let make_zone =
                match read_header ic with
                | `V1 ->
                  input_tz_file_v1 ic
                | `V2 ->
                  input_tz_file_v2 ic
              in
              let digest = Digest.file filename in
              let r = make_zone zonename ~original_filename:filename ~digest in
              r)
          with
          | Invalid_file_format reason ->
            raise (Invalid_file_format (sprintf "%s - %s" filename reason))
        ;;
      end

      let of_utc_offset ~hours:offset =
        assert (offset >= -24 && offset <= 24);
        let name =
          if offset = 0 then "UTC"
          else sprintf "UTC%s%d" (if offset < 0 then "-" else "+") (abs offset)
        in
        let utc_off = Time0.Span.of_int63_seconds (Int63.of_int (offset * 60 * 60)) in
        {
          name                    = name;
          original_filename       = None;
          digest                  = None;
          transitions             = [||];
          last_regime_index       = 0;
          default_local_time_type = {Regime.
                                      utc_off;
                                      is_dst  = false;
                                      abbrv   = name;
                                    };
          leap_seconds = []
        }
      ;;
    end
  end

  include Stable.V1

  let input_tz_file = Zone_file.input_tz_file

  let utc = of_utc_offset ~hours:0

  let clock_shift_at zone i =
    let previous_shift =
      if i = 0
      then zone.default_local_time_type.utc_off
      else zone.transitions.(i - 1).new_regime.utc_off
    in
    ( zone.transitions.(i).start_time
    , (Time0.Span.(-)
         zone.transitions.(i).new_regime.utc_off
         previous_shift)
    )

  let next_clock_shift zone ~after =
    let segment_of (transition : Transition.t) =
      if Time0.(>) transition.start_time after
      then `Right
      else `Left
    in
    Option.map (Array.binary_search_segmented zone.transitions ~segment_of `First_on_right)
      ~f:(fun i -> clock_shift_at zone i)
  ;;

  let prev_clock_shift zone ~before =
    let segment_of (transition : Transition.t) =
      if Time0.(<) transition.start_time before
      then `Left
      else `Right
    in
    Option.map (Array.binary_search_segmented zone.transitions ~segment_of `Last_on_left)
      ~f:(fun i -> clock_shift_at zone i)
  ;;

  let convert_transition (transition : Transition.t) transtype =
    match transtype with
    | `UTC   -> transition.start_time
    | `Local -> Time0.add transition.start_time transition.new_regime.utc_off
  ;;

  (* Determine if [time] is governed by the regime in [transitions.(index)]. *)
  let in_transition transitions ~index time transtype =
    try
      let s = convert_transition transitions.(index) transtype in
      let e = convert_transition transitions.(index + 1) transtype in
      s <= time && time < e
    with
    | _ -> false
  ;;

  (* [find_local_regime zone `UTC time] finds the local time regime in force
     in [zone] at [seconds], from 1970/01/01:00:00:00 UTC.

     [find_local_regime zone `Local seconds] finds the local time regime in force in
     [zone] at [seconds], from 1970/01/01:00:00:00 of [zone].
  *)
  let find_local_regime zone transtype time =
    let module T = Transition in
    let transitions     = zone.transitions in
    let num_transitions = Array.length transitions in
    if num_transitions = 0 then
      zone.default_local_time_type
    else if transitions.(0).T.start_time > time then
      zone.default_local_time_type
    else begin
      if in_transition transitions ~index:zone.last_regime_index time transtype
      then transitions.(zone.last_regime_index).new_regime
      else begin
        let segment_of (transition : Transition.t) =
          let start_time = convert_transition transition transtype in
          if time >= start_time
          then `Left
          else `Right
        in
        let index =
          Option.value_exn
            (Array.binary_search_segmented transitions ~segment_of `Last_on_left)
        in
        zone.last_regime_index <- index;
        transitions.(index).new_regime
      end
    end
  ;;

  let shift_epoch_time zone repr_type epoch =
    let r = find_local_regime zone repr_type epoch in
    match repr_type with
    | `Local -> Time0.sub epoch r.Regime.utc_off
    | `UTC   -> Time0.add epoch r.Regime.utc_off
  ;;

  let abbreviation zone time =
    (find_local_regime zone `UTC time).Regime.abbrv
  ;;

  let name zone = zone.name
end
