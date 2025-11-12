module Condition = Condition
module Event = Event
module Mutex = Mutex
module Semaphore = Semaphore
module Multicore = Multicore

module Thread = struct
  let[@warning "-unused-value-declaration"] use_domains () = ()

  include Thread
end
