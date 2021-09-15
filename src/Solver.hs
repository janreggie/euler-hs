module Solver (solver) where

import Solutions.P1 (p1)
import Solutions.P2 (p2)
import Solutions.P3 (p3)

-- | wrapper for all Solution functions
solver :: Integer -> Maybe String -> Integer
solver 1 = p1
solver 2 = p2
solver 3 = p3
solver _ = error "Unimplemented"
