[%%import "config.h"]
open! Import
open  Int63.Replace_polymorphic_compare

[%%ifdef JSC_ARCH_SIXTYFOUR]
external nanoseconds_since_epoch_or_zero : unit -> Int63.t
  = "core_kernel_time_ns_gettime_or_zero" [@@noalloc]
[%%else]
external nanoseconds_since_epoch_or_zero : unit -> Int63.t
  = "core_kernel_time_ns_gettime_or_zero"
[%%endif]

[%%ifdef JSC_POSIX_TIMERS]
let [@inline never] gettime_failed () = failwith "clock_gettime(CLOCK_REALTIME) failed"
[%%else]
let [@inline never] gettime_failed () = failwith "gettimeofday failed"
[%%endif]

let nanoseconds_since_epoch () =
  let t = nanoseconds_since_epoch_or_zero () in
  if t <> Int63.zero then t else gettime_failed ()
;;
