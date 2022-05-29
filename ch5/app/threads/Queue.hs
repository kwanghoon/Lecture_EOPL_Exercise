module Queue where

type Queue a = [a]

empty_queue :: Queue a
empty_queue = []

isempty :: Queue a -> Bool
isempty queue = null queue

enqueue :: Queue a -> a -> Queue a
enqueue queue elem = queue ++ [elem]

dequeue :: Queue a -> (a -> Queue a -> b) -> b 
dequeue queue f = f (head queue) (tail queue)
  -- Note that dequeue is used in run_next_thread:Schedule.hs and
  -- singal_mutex:Semaphore.hs where
  --
  --    b = Store -> SchedState -> (FinalAnswer, Store).
  --
  -- One can implement dequeue as usual
  --
  --    dequeue :: Queue a -> (a, Queue a)
  --
  -- so that dequeue queue f can be rewritten as
  --
  --    let (head, queue') = dequeue queue in
  --      f head queue'.
  --
  
  

