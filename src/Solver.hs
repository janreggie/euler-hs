module Solver (solver) where

import Solutions.P1 (p1)
import Solutions.P10 (p10)
import Solutions.P11 (p11)
import Solutions.P12 (p12)
import Solutions.P13 (p13)
import Solutions.P2 (p2)
import Solutions.P3 (p3)
import Solutions.P4 (p4)
import Solutions.P5 (p5)
import Solutions.P6 (p6)
import Solutions.P7 (p7)
import Solutions.P8 (p8)
import Solutions.P9 (p9)

-- | wrapper for all Solution functions
solver :: Integer -> String -> Integer
solver 1 = p1
solver 2 = p2
solver 3 = p3
solver 4 = p4
solver 5 = p5
solver 6 = p6
solver 7 = p7
solver 8 = p8
solver 9 = p9
solver 10 = p10
solver 11 = p11
solver 12 = p12
solver 13 = p13
solver _ = error "Unimplemented"
