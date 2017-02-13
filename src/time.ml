(* See Time_float.ml for the primary instantiation of this functor that is visible outside
   of Core_kernel as Time (see core_kernel.ml and std.ml). *)
open !Import
open Std_internal

include Time_intf

module Make (Time0 : Time0_intf.S) = struct
  module Time0 = Time0

  include Time0

  let epoch = of_span_since_epoch (Span.of_sec 0.0)

  module Gmtime : sig
    module Parts : sig
      type t =
        { date  : Date0.t
        ; ofday : Ofday.t }
    end

    val gmtime : t -> Parts.t
  end = struct
    module Parts = struct
      type t =
        { date  : Date0.t
        ; ofday : Ofday.t }
      [@@deriving compare, sexp_of]
    end

    open Int.O

    (* here to help the inliner with days_per_month *)
    let [@inline never] invalid_month month =
      raise_s [%message
        "invalid month passed to [days_per_month]"
          (month : int)]
    ;;

    let [@inline always] days_per_month ~month ~is_leap_year =
      match month with
      | 1  -> 31
      | 2  -> if is_leap_year then 29 else 28
      | 3  -> 31
      | 4  -> 30
      | 5  -> 31
      | 6  -> 30
      | 7  -> 31
      | 8  -> 31
      | 9  -> 30
      | 10 -> 31
      | 11 -> 30
      | 12 -> 31
      | _  -> invalid_month month
    ;;

    let is_leap_year ~year = Date0.is_leap_year ~year

    let year_size year =
      if is_leap_year ~year
      then 366
      else 365
    ;;

    (* This is a faithless recreation of the algorithm used by gmtime in glibc
       (see time/offtime.c in any recent version of glibc).  This accounts for the lack of
       clarity of the algorithm when compared to a simpler looping approach through year
       sized chunks of days.  Unfortunately, the more naive algorithm is meaningfully
       slower. *)
    let [@inline always] calculate_year_and_day_of_year ~days_from_epoch =
      let div a b = a / b - (if Int.rem a b < 0 then 1 else 0) in
      let num_leaps_thru_end_of year = div year 4 - div year 100 + div year 400 in
      let days = ref days_from_epoch in
      let year = ref 1970 in
      while !days < 0 || !days >= year_size !year do
        let year_guess = !year + div !days 365 in
        days := !days - ((year_guess - !year) * 365
                         + num_leaps_thru_end_of (year_guess - 1)
                         - num_leaps_thru_end_of (!year - 1));
        year := year_guess;
      done;
      !year, !days + 1
    ;;

    (* Written with refs and a loop for speed.  Comparision with a let rec loop version
       showed that this was faster. *)
    let [@inline always] calculate_month_and_day_of_month ~day_of_year ~year =
      let month        = ref 1 in
      let days_left    = ref day_of_year in
      let is_leap_year = is_leap_year ~year in
      while !days_left > days_per_month ~month:!month ~is_leap_year do
        days_left := !days_left - days_per_month ~month:!month ~is_leap_year;
        incr month;
      done;
      (!month, !days_left)
    ;;

    (* a recreation of the system call gmtime specialized to the fields we need that also
       doesn't rely on Unix. *)
    let gmtime t : Parts.t =
      let days_from_epoch, ofday_span = to_days_since_epoch_and_remainder t in
      let year, day_of_year   = calculate_year_and_day_of_year ~days_from_epoch in
      let month, day_of_month = calculate_month_and_day_of_month ~day_of_year ~year in
      let date  = Date0.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day_of_month in
      let ofday = Ofday.of_span_since_start_of_day ofday_span in
      { date; ofday }
    ;;

    let%test_unit "negative time" =
      let t = of_span_since_epoch (Span.of_sec (-86_399.)) in
      [%test_result: Parts.t]
        ~expect:{ date = Date0.of_string "1969-12-31"
                ; ofday = Ofday.of_span_since_start_of_day Span.second
                }
        (gmtime t)
  end

  let is_earlier t1 ~than:t2 = t1 <. t2
  let is_later   t1 ~than:t2 = t1 >. t2

  module Zone = Zone.Make (Time0)

  let abs_diff t1 t2 = Span.abs (diff t1 t2)

  let of_date_ofday ~zone date ofday =
    let epoch = utc_mktime date ofday in
    Zone.shift_epoch_time zone `Local epoch
  ;;

  let of_date_ofday_precise date ofday ~zone =
    (* We assume that there will be only one zone shift within a given local day.  *)
    let start_of_day = of_date_ofday ~zone date Ofday.start_of_day in
    let proposed_time = add start_of_day (Ofday.to_span_since_start_of_day ofday) in
    match Zone.next_clock_shift zone ~after:start_of_day with
    | None -> `Once proposed_time
    | Some (shift_start, shift_amount) ->
      let shift_backwards = Span.(shift_amount < zero) in
      (* start and end of the "problematic region" *)
      let s,e =
        if shift_backwards
        then add shift_start shift_amount, shift_start
        else shift_start, add shift_start shift_amount
      in
      if proposed_time < s then
        `Once proposed_time
      else if s <= proposed_time && proposed_time < e then begin
        if shift_backwards
        then `Twice (proposed_time, sub proposed_time shift_amount)
        else `Never shift_start
      end else
        `Once (sub proposed_time shift_amount)
  ;;

  module Epoch_cache = struct
    type nonrec t = {
      zone      : Zone.t;
      day_start : t;
      day_end   : t;
      date      : Date0.t
    }
  end

  let date_ofday_of_epoch_internal zone (time : t) =
    let {Gmtime.Parts. date; ofday} = Gmtime.gmtime time in
    let day_start = sub time (Ofday.to_span_since_start_of_day ofday) in
    let day_end   = add day_start Span.day in
    let cache     = {Epoch_cache. zone; day_start; day_end; date} in
    (cache, (date, ofday))
  ;;

  (* A thin caching layer over the actual date_ofday_of_epoch
     (date_ofday_of_epoch_internal just above) used only to gain some speed when we
     translate the same time/date over and over again *)
  let date_ofday_of_epoch =
    let cache = ref (fst (date_ofday_of_epoch_internal Zone.utc (now ()))) in
    (fun zone unshifted ->
       (* If at time T you are +H hours from UTC, then the correct local time at T is the
          time in UTC at T, plus an offset of H hours. But since UTC is nice and simple,
          that's the same as what the time in UTC will be in H hours' time (if only it were
          always that simple!) *)
       let time = Zone.shift_epoch_time zone `UTC unshifted in
       let {Epoch_cache.zone = z; day_start = s; day_end = e; date = date} = !cache in
       if phys_equal zone z && time >= s && time < e then (
         (date, Ofday.of_span_since_start_of_day (diff time s)))
       else begin
         let (new_cache, r) = date_ofday_of_epoch_internal zone time in
         cache := new_cache;
         r
       end)
  ;;

  let to_date_ofday time ~zone = date_ofday_of_epoch zone time
  ;;

  (* The correctness of this algorithm (interface, even) depends on the fact that
     timezone shifts aren't too close together (as in, it can't simultaneously be the
     case that a timezone shift of X hours occurred less than X hours ago, *and*
     a timezone shift of Y hours will occur in less than Y hours' time) *)
  let to_date_ofday_precise time ~zone =
    let date, ofday       = to_date_ofday time ~zone in
    let clock_shift_after = Zone.next_clock_shift zone ~after:time in
    let clock_shift_before_or_at =
      let after_time_but_not_after_next =
        match clock_shift_after with
        | None                 -> add time Span.second
        | Some (next_start, _) -> next_start
      in
      Zone.prev_clock_shift zone ~before:after_time_but_not_after_next
    in
    let also_skipped_earlier amount =
      (* Using [date] and raising on [None] here is OK on the assumption that clock
         shifts can't cross date boundaries. This is true in all cases I've ever heard
         of (and [of_date_ofday_precise] would need revisiting if it turned out to be
         false) *)
      match Ofday.sub ofday amount with
      | Some ofday -> `Also_skipped (date, ofday)
      | None       ->
        raise_s [%message
          "Time.to_date_ofday_precise"
            ~span_since_epoch:(to_span_since_epoch time : Span.t)
            (zone : Zone.t)]
    in
    let ambiguity =
      (* Edge cases: the instant of transition belongs to the new zone regime. So if the
         clock moved by an hour exactly one hour ago, there's no ambiguity, because the
         hour-ago time belongs to the same regime as you, and conversely, if the clock
         will move by an hour in an hours' time, there *is* ambiguity. Hence [>.] for
         the first case and [<=.] for the second. *)
      match clock_shift_before_or_at, clock_shift_after with
      | Some (start, amount), _ when (>.) (add start (Span.abs amount)) time ->
        (* clock shifted recently *)
        if Span.(amount > zero) then
          (* clock shifted forward recently: we skipped a time *)
          also_skipped_earlier amount
        else begin
          (* clock shifted back recently: this date/ofday already happened *)
          assert Span.(amount < zero);
          `Also_at (sub time (Span.abs amount))
        end
      | _, Some (start, amount) when (<=.) (sub start (Span.abs amount)) time ->
        (* clock is about to shift *)
        if Span.(amount > zero) then
          (* clock about to shift forward: no effect *)
          `Only
        else begin
          (* clock about to shift back: this date/ofday will be repeated *)
          assert Span.(amount < zero);
          `Also_at (add time (Span.abs amount))
        end
      | _ -> `Only
    in
    date, ofday, ambiguity
  ;;

  let to_date t ~zone  = fst (to_date_ofday t ~zone)
  let to_ofday t ~zone = snd (to_date_ofday t ~zone)

  let convert ~from_tz ~to_tz date ofday =
    let start_time = of_date_ofday ~zone:from_tz date ofday in
    date_ofday_of_epoch to_tz start_time
  ;;

  let utc_offset t ~zone =
    let utc_epoch = Zone.shift_epoch_time zone `UTC t in
    diff utc_epoch t
  ;;

  let offset_string time ~zone =
    let utc_offset   = utc_offset time ~zone in
    let is_utc       = Span.(=) utc_offset Span.zero in
    if is_utc
    then "Z"
    else
      String.concat
        [ (if Span.(<) utc_offset Span.zero then "-" else "+");
          Ofday.to_string_trimmed
            (Ofday.of_span_since_start_of_day (Span.abs utc_offset));
        ]
  ;;

  let to_string_abs_parts time ~zone =
    let date, ofday   = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    [ Date0.to_string date;
      String.concat ~sep:"" [ Ofday.to_string ofday; offset_string ]
    ]
  ;;

  let to_string_abs_trimmed time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    String.concat ~sep:" "
      [ (Date0.to_string date)
      ; (Ofday.to_string_trimmed ofday) ^ offset_string
      ]
  ;;

  let to_string_abs time ~zone =
    String.concat ~sep:" " (to_string_abs_parts ~zone time)
  ;;

  let to_string t = to_string_abs t ~zone:Zone.utc

  let to_string_iso8601_basic time ~zone =
    String.concat ~sep:"T" (to_string_abs_parts ~zone time)

  let to_string_trimmed t ~zone =
    let date, sec = to_date_ofday ~zone t in
    (Date0.to_string date) ^ " " ^ (Ofday.to_string_trimmed sec)
  ;;

  let to_sec_string t ~zone =
    let date, sec = to_date_ofday ~zone t in
    (Date0.to_string date) ^ " " ^ (Ofday.to_sec_string sec)
  ;;

  let to_filename_string t ~zone =
    let date, ofday = to_date_ofday ~zone t in
    (Date0.to_string date) ^ "_" ^
    (String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday))
  ;;

  let of_filename_string s ~zone =
    try
      match String.lsplit2 s ~on:'_' with
      | None -> failwith "no space in filename string"
      | Some (date, ofday) ->
        let date = Date0.of_string date in
        let ofday = String.tr ~target:'-' ~replacement:':' ofday in
        let ofday = Ofday.of_string ofday in
        of_date_ofday date ofday ~zone
    with
    | exn ->
      invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
  ;;

  let of_localized_string ~zone str =
    try
      match String.lsplit2 str ~on:' ' with
      | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
      | Some (date,time) ->
        let date  = Date0.of_string date in
        let ofday = Ofday.of_string time in
        of_date_ofday ~zone date ofday
    with e ->
      Exn.reraise e "Time.of_localstring"
  ;;

  let occurrence before_or_after t ~ofday ~zone =
    let first_guess_date = to_date t ~zone in
    let first_guess      = of_date_ofday ~zone first_guess_date ofday in
    let cmp, increment =
      match before_or_after with
      | `Last_before_or_at -> (<=), (-1)
      | `First_after_or_at -> (>=), 1
    in
    if cmp first_guess t
    then first_guess
    else of_date_ofday ~zone (Date0.add_days first_guess_date increment) ofday
  ;;

  let ensure_colon_in_offset offset =
    if Char.(=) offset.[1] ':'
    || Char.(=) offset.[2] ':'
    then offset
    else begin
      let offset_length = String.length offset in
      if Int.(<) offset_length 3 || Int.(>) offset_length 4
      then failwithf "invalid offset %s" offset ()
      else String.concat
             [ String.slice offset 0 (offset_length - 2)
             ; ":"
             ; String.slice offset (offset_length - 2) offset_length ]
    end
  ;;

  exception Time_of_string of string * Exn.t [@@deriving sexp]

  let of_string_gen ~default_zone ~find_zone s =
    try
      let date,ofday,tz =
        match String.split s ~on:' ' with
        | [day; month; year; ofday] ->
          (String.concat [day; " "; month; " "; year], ofday, None)
        | [date; ofday; tz] -> (date, ofday, Some tz)
        | [date; ofday]     -> (date, ofday, None)
        | [s]              ->
          begin match String.rsplit2 ~on:'T' s with
          | Some (date, ofday) -> (date, ofday, None)
          | None -> failwith "no spaces or T found"
          end
        | _ -> failwith "too many spaces"
      in
      let ofday_to_sec od = Span.to_sec (Ofday.to_span_since_start_of_day od) in
      let ofday,utc_offset =
        match tz with
        | Some _ -> ofday, None
        | None   ->
          if Char.(=) ofday.[String.length ofday - 1] 'Z'
          then (String.sub ofday ~pos:0 ~len:(String.length ofday - 1), Some 0.)
          else begin
            match String.lsplit2 ~on:'+' ofday with
            | Some (l, r) ->
              (l, Some (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r))))
            | None ->
              begin match String.lsplit2 ~on:'-' ofday with
              | Some (l, r) ->
                ( l
                , Some ((-1.)
                        *. (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))))
              | None       -> ofday, None
              end
          end
      in
      let date  = Date0.of_string date in
      let ofday = Ofday.of_string ofday in
      match tz with
      | Some tz -> of_date_ofday ~zone:(find_zone tz) date ofday
      | None ->
        match utc_offset with
        | None            ->
          let zone = default_zone () in
          of_date_ofday ~zone date ofday
        | Some utc_offset ->
          let utc_t = of_date_ofday ~zone:Zone.utc date ofday in
          sub utc_t (Span.of_sec utc_offset)
    with
    | e -> raise (Time_of_string (s,e))
  ;;

  let of_string s =
    let default_zone () = Zone.utc in
    let find_zone zone_name =
      failwithf "unable to lookup Zone %s.  Try using Core.Time.of_string" zone_name ()
    in
    of_string_gen ~default_zone ~find_zone s
  ;;

  let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
    if Span.(<=) interval Span.zero
    then failwiths "Time.next_multiple got nonpositive interval" interval
           [%sexp_of: Span.t];
    let base_to_after = diff after base in
    if Span.(<) base_to_after Span.zero
    then base (* [after < base], choose [k = 0]. *)
    else begin
      let next =
        add base
          (Span.scale interval
             (Float.round ~dir:`Down (Span.(//) base_to_after interval)))
      in
      if next > after || (can_equal_after && next = after)
      then next
      else add next interval
    end
  ;;
end
