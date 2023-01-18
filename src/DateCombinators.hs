{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
     
module DateCombinators
  (module DateCombinators
  ,module S
  ) where

import           Chronos
import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           DateCombinators.BusinessDay as S
import           DateCombinators.Utils as S hiding (lm2, lm)
import           Interval
import           Yahp

-- * Types

type EDay = Extended Day

data DateRange = DateRange { drStart    :: EDay
                           , drEnd      :: EDay
                           }

instance Show DateRange where
  show (DateRange s e) = show $ on (,) (_Finite %~ showDay) s e

invert :: DateRange -> DateRange
invert (DateRange s e) = DateRange e s


type NthExtractor = Int -> DateRange -> Chronos.Day 

type RangeConstructor = Day -> DateRange

-- * Helper functions

-- | expects a function that creates an ordered list (with the same order of [start, end])
-- of days starting on or after the drStart of the range
buildNthFromEnum :: HasCallStack => (DateRange -> [Day]) -> NthExtractor
buildNthFromEnum enum i = g (abs i) . enum . if i < 0 then invert else id
  where g 0 _     = throw ZeroIndex
        g 1 (x:_) = x
        g i (_:r) = g (pred i) r
        g _ []    = throw $ IndexOutOfRange $ show i

-- | the constructor should move within a given set of dates, according in the following way
-- `gen n day` will move `n-1` steps forward starting from the first valid date AFTER `day` 
buildNthFromDirectConstructor :: HasCallStack => (Int -> Day -> Day) -> NthExtractor
buildNthFromDirectConstructor gen i' (DateRange s e) | i'>0  = check (<=) e s
                                                     | i'<0  = check (>=) s e
                                                     | True = throw ZeroIndex
  where (i, opT) = if s > e then (-i', flip) else (i', id)
        check op lm = \case Finite v -> let res = if i>0 then gen i $ addDays (-1) v else gen (i+1) v in
                                          bool res (throw $ IndexOutOfRange $ show i') $ fmap (opT op res) (fromExtended lm) == Just False
                            _ -> throw InfinitDate
        check :: (Day -> Day -> Bool) -> (Extended Day) -> (Extended Day) -> Day

-- traceEDay x = traceShow (showDay <$> x) x
-- traceDay x = traceShow (showDay x) x
         



-- * Range Constructors


jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec :: Year -> DateRange
[jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec] = everyMonth'

everyMonth' :: [Year -> DateRange]
everyMonth' = g <$> [january..december]
  where g m y = month $ dateToDay $ Date y m 1

everyMonth :: Year -> [DateRange]
everyMonth = sequence everyMonth'

everyQuarter' :: [Year -> DateRange]
everyQuarter' = g <$> [january, april, july, october]
  where g m y = DateRange (Finite start) $ Finite $ addDays (-1) $ addGregorianMonths 3 start
          where start = dateToDay $ Date y m 1

everyQuarter :: Year -> [DateRange]
everyQuarter = sequence everyQuarter'

q1, q2, q3, q4 :: Year -> DateRange
[q1, q2, q3, q4] = everyQuarter'

onOrAfter, after, before, onOrBefore :: RangeConstructor
onOrAfter       d = DateRange (Finite d)                PositiveInfinity
after           d = DateRange (Finite $ succ d)         PositiveInfinity
onOrBefore      d = DateRange (Finite d)                NegativeInfinity 
before          d = DateRange (Finite $ pred d)         NegativeInfinity 

month :: RangeConstructor
month d = DateRange (Finite start) $ Finite $ addDays (-1) $ addGregorianMonths 1 start
  where start = d & _dayToDate . _dateDay .~ 1

-- * Nth Extractors

weekday :: NthExtractor
weekday = buildNthFromDirectConstructor addWeekdays

day :: NthExtractor
day = buildNthFromDirectConstructor addDays

businessDay' :: HasCallStack => HolidayCalendar -> NthExtractor
businessDay' = buildNthFromDirectConstructor . addBusinessDays

businessDay :: HasCallStack => HolidayCalendars -> Text -> NthExtractor
businessDay cals name = maybe (throw $ HolidayCalenderNotAvailable name)  businessDay' $ cals HM.!? name

mon, tue, wed, thu, fri, sat, sun :: HasCallStack => NthExtractor
[sun, mon, tue, wed, thu, fri, sat] = buildNthFromDirectConstructor . flip nextNthWeekday . DayOfWeek <$>
  [0..6]


addGregorianMonths' :: Int -> Date -> Date
addGregorianMonths' i (Date (Year y) (Month m) d) = Date (Year $ y + dy) (Month m') d
          where (dy, m') = divMod (m + i) 12


-- | tests
-- λ> showDay $ Day 2
-- "1858.11.19"
-- λ> showDay $ addGregorianMonths 2 $ Day 2
-- "1859.01.19"
-- λ> showDay $ addGregorianMonths (-11) $ Day 2
-- "1857.12.19"
addGregorianMonths :: Int -> Day -> Day
addGregorianMonths i = _dayToDate %~ addGregorianMonths' i

addWeekdays :: Int -> Day -> Day
addWeekdays n day = addDays (7*weeks + bdays - wd + 1) day
  where (weeks, bdays) = divMod (n + min 5 wd - 1) 5
        wd = transformSundayTo7 day

newtype WeekdayNumber = WeekdayNumber { getWeekdayNumber :: Int }
  deriving (Real, Num, Ord, Eq, Integral, Enum, Show)

weekdayNumber :: Day -> WeekdayNumber
weekdayNumber d = let (w,r) = divMod (2 + getDay d) 7 in WeekdayNumber $ w*5 + min 4 r

-- | gives the expected result when both days are weekdays
-- 
-- if an input is on a weekend, the result is the same as if substituted by the previou friday
diffWeekdays :: Day -> Day -> Int
diffWeekdays = on (-) $ getWeekdayNumber . weekdayNumber

transformSundayTo7 :: Day -> Int
transformSundayTo7 d = case getDayOfWeek $ dayOfWeek d of { 0 -> 7 ; x -> x }

nextNthWeekday :: Int -> DayOfWeek -> Day -> Day
nextNthWeekday n target day = addDays (7*(n-1) + days) day
  where days = ((7 + getDayOfWeek target - getDayOfWeek (dayOfWeek day) - 1) `mod` 7) + 1
  

catchDayError :: MonadError Text m => (x -> Day) -> x -> m x
catchDayError f x = catchWhnf (\e -> toS $ show (e::DateCombinatorsException)) $ seq (f x) x

catchFoldDayError :: (Container f, MonadError Text m) => (x -> Day) -> f x -> m (f x)
catchFoldDayError = mapErrors . catchDayError


weekdayNumberFrac :: Fractional a => Day -> a
weekdayNumberFrac d = let (w,r) = divMod (2 + getDay d) 7 in fromIntegral (w*5) +
  case r of
    5 -> 13/3
    6 -> 14/3
    x -> fromIntegral x
{-# INLINABLE weekdayNumberFrac #-}
{-# SPECIALIZE weekdayNumberFrac :: Day -> Double #-}
{-# SPECIALIZE weekdayNumberFrac :: Day -> Float #-}
{-# SPECIALIZE weekdayNumberFrac :: Day -> Rational #-}
