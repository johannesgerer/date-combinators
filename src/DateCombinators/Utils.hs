
module DateCombinators.Utils where

import           Chronos
import           Yahp

showDay :: Day -> Text
showDay = encode_Ymd (Just '.') . dayToDate

deriving instance Num Year

deriving instance Num DayOfMonth

instance Num Month where
  (+) = lm2 (+)
  (*) = lm2 (*)
  negate = lm negate
  abs = lm abs
  signum = lm signum
  fromInteger = Month . pred . fromInteger -- The reason for this manual instance !! 


lm2 f (Month a) (Month b) = Month $ f a b
lm f (Month a) = Month $ f a



data DateCombinatorsException = IndexOutOfRange String
                              | InfinitDate
                              | ZeroIndex
                              | DayNotInCalendar Text
                              | HolidayCalenderNotAvailable Text
                              | ParseError String
                              | DynamicTypeMismatch String
                              deriving (Generic, Eq)

instance Show  DateCombinatorsException where
  show = \case
    IndexOutOfRange s                -> "IndexOutOfRange: " <> s
    InfinitDate                      -> "InfinitDate"
    ZeroIndex                        -> "ZeroIndex"
    DayNotInCalendar s               -> "DayNotInCalendar: " <> toS s
    ParseError s                     -> "ParseError: " <> s
    DynamicTypeMismatch s            -> "DynamicTypeMismatch: " <> s
    HolidayCalenderNotAvailable s    -> "HolidayCalenderNotAvailable: " <> toS s

instance Exception DateCombinatorsException
