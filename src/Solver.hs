module Solver (solver) where

import Solutions.P1 (p1)

-- | wrapper for all Solution functions
solver :: Integer -> Maybe String -> Integer
solver 1 = p1
solver _ = error "Unimplemented"
