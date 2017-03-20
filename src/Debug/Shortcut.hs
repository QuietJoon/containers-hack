module Debug.Shortcut where

import Debug.Trace

tm = False
traceX str x = if tm then trace str x else x
traceS :: Show a => a -> x -> x
traceS a x = if tm then traceShow a x else x
