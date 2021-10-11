module Solutions.P19 (p19) where

type Year = Int

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Ord, Show)

instance Enum Month where
  toEnum 0 = January
  toEnum 1 = February
  toEnum 2 = March
  toEnum 3 = April
  toEnum 4 = May
  toEnum 5 = June
  toEnum 6 = July
  toEnum 7 = August
  toEnum 8 = September
  toEnum 9 = October
  toEnum 10 = November
  toEnum 11 = December
  toEnum x = toEnum (((x -1) `mod` 12) + 1)

  fromEnum January = 0
  fromEnum February = 1
  fromEnum March = 2
  fromEnum April = 3
  fromEnum May = 4
  fromEnum June = 5
  fromEnum July = 6
  fromEnum August = 7
  fromEnum September = 8
  fromEnum October = 9
  fromEnum November = 10
  fromEnum December = 11

data YearMonth = YearMonth Year Month deriving (Eq, Ord)

instance Show YearMonth where
  show (YearMonth yyyy mm) = show mm ++ " " ++ show yyyy

instance Enum YearMonth where
  toEnum n = YearMonth (n `div` 12) (toEnum (n `mod` 12) :: Month)
  fromEnum (YearMonth yyyy month) = fromEnum month + (yyyy * 12)

-- instance Eq YearMonth where
--   (YearMonth y1 m1) == (YearMonth y2 m2) = y1 == y2 && m1 == m2

-- instance Ord YearMonth where
--   compare (YearMonth y1 m1) (YearMonth y2 m2)
--     | y1 < y2 = LT
--     | y1 > y2 = GT
--     | otherwise = compare m1 m2

countDays :: YearMonth -> Int
countDays (YearMonth _ January) = 31
countDays (YearMonth yyyy February)
  | yyyy `mod` 400 == 0 = 29
  | yyyy `mod` 100 == 0 && yyyy `mod` 400 /= 0 = 28
  | yyyy `mod` 4 == 0 = 29
  | otherwise = 28
countDays (YearMonth _ March) = 31
countDays (YearMonth _ April) = 30
countDays (YearMonth _ May) = 31
countDays (YearMonth _ June) = 30
countDays (YearMonth _ July) = 31
countDays (YearMonth _ August) = 31
countDays (YearMonth _ September) = 30
countDays (YearMonth _ October) = 31
countDays (YearMonth _ November) = 30
countDays (YearMonth _ December) = 31

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show)

instance Enum Day where
  fromEnum Sunday = 0
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6

  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum x = toEnum (x `mod` 7)

-- A particular YearMonth starts with day Day
data YearMonthDay = YMD YearMonth Day

instance Show YearMonthDay where
  show (YMD ym d) = show ym ++ ": " ++ show d

next :: YearMonthDay -> YearMonthDay
next (YMD ym day) = YMD (succ ym) (next day)
  where
    next :: Day -> Day
    next day = add day (countDays ym)

    add :: Day -> Int -> Day
    add day n = iterate succ day !! (n `mod` 7)

p19 :: String -> Integer
p19 _ = toInteger . length . filter startsOnSunday $ takeWhile inTwentiethCE $ dropWhile (not . inTwentiethCE) (iterate next (YMD (YearMonth 1900 January) Monday))
  where
    startsOnSunday (YMD _ day) = day == Sunday
    inTwentiethCE (YMD ym _) = ym >= YearMonth 1901 January && ym <= YearMonth 2000 December
