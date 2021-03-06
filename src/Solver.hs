module Solver (solver) where

import Solutions.P1 (p1)
import Solutions.P10 (p10)
import Solutions.P11 (p11)
import Solutions.P12 (p12)
import Solutions.P13 (p13)
import Solutions.P14 (p14)
import Solutions.P15 (p15)
import Solutions.P16 (p16)
import Solutions.P17 (p17)
import Solutions.P18 (p18)
import Solutions.P19 (p19)
import Solutions.P2 (p2)
import Solutions.P20 (p20)
import Solutions.P21 (p21)
import Solutions.P22 (p22)
import Solutions.P23 (p23)
import Solutions.P24 (p24)
import Solutions.P25 (p25)
import Solutions.P26 (p26)
import Solutions.P28 (p28)
import Solutions.P3 (p3)
import Solutions.P30 (p30)
import Solutions.P38 (p38)
import Solutions.P4 (p4)
import Solutions.P41 (p41)
import Solutions.P45 (p45)
import Solutions.P48 (p48)
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
solver 14 = p14
solver 15 = p15
solver 16 = p16
solver 17 = p17
solver 18 = p18
solver 19 = p19
solver 20 = p20
solver 21 = p21
solver 22 = p22
solver 23 = p23
solver 24 = p24
solver 25 = p25
solver 26 = p26
solver 28 = p28
solver 30 = p30
solver 38 = p38
solver 41 = p41
solver 45 = p45
solver 48 = p48
solver _ = error "Unimplemented"
