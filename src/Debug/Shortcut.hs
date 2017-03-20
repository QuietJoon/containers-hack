module Debug.Shortcut where

import Debug.Trace

tm :: Bool
tm = False

traceX :: String -> x -> x
traceX str x = if tm then trace str x else x
traceS :: Show a => a -> x -> x
traceS a x = if tm then traceShow a x else x
