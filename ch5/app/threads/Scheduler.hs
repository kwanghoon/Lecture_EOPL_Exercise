module Scheduler where

import EnvStore
import Queue
import Data.Maybe

--
timeslice = 5

--
initialize_scheduler :: Integer -> SchedState
initialize_scheduler ticks =
  SchedState {
   the_ready_queue = empty_queue,
   the_final_answer = Nothing,
   the_max_time_slice = ticks,
   the_time_remaining = ticks
  }

place_on_ready_queue :: Thread -> SchedState -> SchedState
place_on_ready_queue th scState =
  scState { the_ready_queue = enqueue (the_ready_queue scState) th }

run_next_thread :: Store -> SchedState -> (FinalAnswer, Store)
run_next_thread store scState =
  if isempty (the_ready_queue scState)
  then (fromJust (the_final_answer scState), store)
  else
    dequeueWithFun (the_ready_queue scState)
     (\first_ready_thread other_ready_threads ->
        first_ready_thread
          store
          ( scState { the_ready_queue = other_ready_threads,
                      the_time_remaining = the_max_time_slice scState } ) )

set_final_answer :: SchedState -> ExpVal -> SchedState
set_final_answer scState val = scState { the_final_answer = Just val }

time_expired :: SchedState -> Bool
time_expired scState = the_time_remaining scState==0

decrement_timer :: SchedState -> SchedState
decrement_timer scState = scState { the_time_remaining = the_time_remaining scState - 1 }
