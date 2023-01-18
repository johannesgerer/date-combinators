
module DateCombinators.Tests where


import           Chronos hiding (day, second)
import           Data.Array
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe as Unsafe
import qualified Data.Set as S
import qualified Data.Time.Calendar as C
import           Data.Time.Calendar.WeekDate hiding (Year)
import           Data.Tuple.Select
import           DateCombinators
import           DateCombinators.Parser
import           Test.Hspec hiding (before, after)
import           Test.QuickCheck
import           Yahp

deriving instance Arbitrary Day

main :: IO ()
main = hspec $ do
  describe "weekdaysBimap" $ do
    it "should be non-empty" $ 
      property $ \x -> not . null . fst . bdBimap . weekdaysBimap x

  describe "dayToWeekday" $ do
      it "should be identical to the `time` version" $ do
       property $ \i -> mod (weekday2 $ C.ModifiedJulianDay i) 7 ===  getDayOfWeek (Chronos.dayOfWeek (Day $ fromIntegral i))

  describe "DateCombinators" $ do
      let cmp i (a,b, c) =  it ("should satisfy case " <> show i) $ shouldBe (diffBusinessDays cal a b) c
          dd = toDay 2020
          cal = Unsafe.fromJust $ generateHolidayCalendar $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
      zipWithM_ cmp [(1::Int)..]
        [(dd 5 3, dd 5 3    , 0)
        ,(dd 5 2, dd 5 2    , 0)
        ,(dd 5 2, dd 5 6    , 1)
        ,(dd 5 1, dd 5 6    , 1)
        ,(dd 4 30, dd 5 6   , 2)
        ,(dd 4 30, dd 5 6   , 2)
        ,(dd 4 30, dd 5 5   , 2)
        ]

  describe "DateCombinators" $ do
      let cmp i (a,b) =  it ("should satisfy case " <> show i) $ on shouldBe showDay a b
          dd = toDay 2020
          cal = Unsafe.fromJust $ generateHolidayCalendar $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
          bday = businessDay' cal
      zipWithM_ cmp [(1::Int)..]
        [(1 `weekday` before (dd april 4)       , dd april 3)
        ,(1 `weekday` after (dd april 4)        , dd april 6)
        ,(1 `weekday` after (dd april 6)        , dd april 7)
        ,(1 `weekday` before (dd april 3)       , dd april 2)
        ,(1 `weekday` onOrBefore (dd april 6)   , dd april 6)
        ,(1 `weekday` onOrBefore (dd april 4)   , dd april 3)
        ,(1 `weekday` onOrBefore (dd april 3)   , dd april 3)
        ,(1 `weekday` onOrAfter (dd april 6)    , dd april 6)
        ,(1 `weekday` onOrAfter (dd april 4)    , dd april 6)
        ,(1 `weekday` onOrAfter (dd april 3)    , dd april 3)
        ,(1 `day` onOrAfter (dd april 3)        , dd april 3)
        ,(1 `day` after (dd april 3)            , dd april 4)
        ,(1 `day` before (dd april 3)           , dd april 2)
        ,(1 `day` onOrBefore (dd april 3)       , dd april 3)
        ,((-1) `day` month (dd april 3)         , dd april 30)
        ,(1 `day` month (dd april 3)            , dd april 1)
        ,(4 `weekday` month (dd april 3)        , dd april 6)
        ,(1 `fri` month (dd april 3)            , dd april 3)
        ,((-1) `fri` month (dd april 3)         , dd april 24)
        ,(1 `wed` month (dd april 3)            , dd april 1)
        ,(5 `wed` month (dd april 3)            , dd april 29)
        ,((-1) `wed` month (dd april 3)         , dd april 29)
        ,((-5) `wed` month (dd april 3)         , dd april 1)
        ,(1 `wed` invert (month $ dd april 3)   , dd april 29)
        ,((-1) `wed` invert (month $ dd april 3), dd april 1)
        ,(1 `bday` (month $ dd 5 3)             , dd 5 2)
        ,(1 `bday` after (1 `bday` (month $ dd 5 3))    , dd 5 6)
        ]

      it "checks bounds" $ shouldThrow (flip seq (pure ()) $ 6 `wed` month (dd april 3)) (== IndexOutOfRange "6")
      it "checks bounds2" $ shouldThrow (flip seq (pure ()) $ (-6) `wed` month (dd april 3)) (== IndexOutOfRange "-6")
      it "checks bounds3" $ shouldThrow (flip seq (pure ()) $ 6 `wed` invert (month $ dd april 3)) (== IndexOutOfRange "6")
      it "checks bounds3" $ shouldThrow (flip seq (pure ()) $ (-6) `wed` invert (month $ dd april 3)) (== IndexOutOfRange "-6")
      it "outside of calendar" $ shouldThrow (flip seq (pure ()) $ 1 `bday` (month $ toDay 2017 april 3)) (\case {DayNotInCalendar _ -> True; _ -> False })

  describe "DateCominators.Parser" $ do
      let cmp i a b =  it ("should satisfy case " <> show i) $ shouldBe a b
          runExprM q = eitherException $ runExprParser multipleFuncWithFinalListParser (HM.fromList [("de", cal1)]) q 
          runExprM2 q = eitherException $ runExprParser multipleFuncWithFinalListParserExec (HM.fromList [("de", cal1)]) q 
          cal1 = Unsafe.fromJust $ generateHolidayCalendar $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2025,1,1)]
            where toDay' (y,m,d) = toDay y m d
      sequence_ $ zipWith3 cmp [(1::Int)..]
        [show (runExprM "jan" $ Year 2022 :: [([Text], DateRange)])
        ,show (runExprM2 "[jan, feb]" $ Year 2022 :: [DateRange])
        ,show (showDay <$> runExprM2 "1 fri [jan, feb]" (Year 2022))
        ,show (showDay <$> runExprM2 "[2 wday after 1 fri jan, -1 wday feb]" (Year 2022))
        ,show (second showDay <$> runExprM "1 bday@de before [2 wday after 1 fri jan, -1 wday feb]" (Year 2022))
        ,show (second showDay <$> runExprM "1 wday onOrBefore 23 day  nov " (Year 1800))
        ]
        ["[([\"jan\"],(Finite \"2022.01.01\",Finite \"2022.01.31\"))]"
        ,"[(Finite \"2022.01.01\",Finite \"2022.01.31\"),(Finite \"2022.02.01\",Finite \"2022.02.28\")]"
        ,"[\"2022.01.07\",\"2022.02.04\"]"
        ,"[\"2022.01.11\",\"2022.02.28\"]"
        ,"[([\"bday\",\"before\",\"wday\",\"after\",\"fri\",\"jan\"],\"2022.01.10\"),([\"bday\",\"before\",\"wday\",\"feb\"],\"2022.02.27\")]"
        ,"[([\"wday\",\"onOrBefore\",\"day\",\"nov\"],\"1800.11.21\")]"
        ]

  describe "generateHolidayCalendar" $ do
    it "satisfies a list of properties" $
      let allWeekDays2 f t = filter isWeekend [f..t]
          prop2 hs = maybe (property True) (prop hs . bdBimap) $ generateHolidayCalendar hs
          prop hs (dToB, bToD) = counterexample (show (holidays, mapping)) $
                                 conjoin [let probs = filter (\(d,bd) -> not $ S.member d holidays ==
                                                               not (isJust $ toBusinessDay bd)) mapping
                                          in counterexample (show probs) $ null probs
                                         ,sortOn snd mapping === sortOn fst mapping
                                         ,numHolidays===length holidays  
                                         ,length dToB===numDays  
                                         ,fromBusinessDay (snd $ bounds bToD)===numBDays  
                                         ,counterexample (show $ length mapping) $  length mapping ===numDays
                                         ]
            where numDays = succ $ on (-) fromEnum (S.findMax holidays) $ S.findMin holidays
                  holidays = S.fromList hs
                  mapping = assocs dToB
                  numBDays = numDays - numHolidays
                  numHolidays = length (filter (not . isJust . toBusinessDay . snd) mapping)
      in property (prop2 . fmap Day) .&&. property (\f t -> prop2 $ allWeekDays2 (Day f) (Day t))

  describe "addBusinessDays" $ do
    let cal = Unsafe.fromJust $ generateHolidayCalendar $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
        cases =                       [ (-1,    (2020, 5, 4), (2020, 4, 30)   )
                                      , (1,     (2020, 5, 4), (2020, 5, 6)    )
                                      , (0,     (2020, 5, 1), (2020, 4, 30)   )
                                      , (0,     (2020, 5, 4), (2020, 5, 2)    )
                                      ]
        cmp i (o,d,res) =  it ("should satisfy case " <> show i <> " for: " <> toS (showDay $ toDay' d)) $ shouldBe
                           (showDay $ addBusinessDays cal o $ toDay' d) $ showDay $ toDay' res
      in zipWithM_ cmp [(1::Int)..] cases

weekday2 :: C.Day -> Int
weekday2 = sel3 . toWeekDate


toDay' :: (Year, Month, DayOfMonth) -> Day
toDay' (y,m,d) = toDay y m d



-- * Example 

ex1 = showDay $ eitherException (runExpr "1 mon month") (toDay 2020 4 9) 

ex2 = eitherException (runExpr "jan") $ Year 2022 :: DateRange

runExpr q = runExprParser multipleFuncParser (HM.fromList [("de", cal1)]) q 

-- runExprM q = showDay $ runExprParser multipleFuncParser (HM.fromList [("de", cal1)]) q (toDay 2020 4 9)

-- runExpr "1 bday@de month 1 monthAfter"
-- "2020.05.02"
-- runExpr "1 bday@de before -2 wday month"
-- "2020.04.28"

-- ex3 = mapM_ (putStrLn . show)
--   [show (runExprM "jan" $ Year 2022 :: [DateRange])
--   ,show (runExprM "[jan, feb, everyMonth]" $ Year 2022 :: [DateRange])
--   ,show (showDay <$> runExprM "1 fri [jan, feb, everyMonth]" (Year 2022))
--   ,show (showDay <$> runExprM "[2 wday after 1 fri jan, -1 wday feb]" (Year 2022))
--   ,show (showDay <$> runExprM "1 bday@de before [2 wday after 1 fri jan, -1 wday feb]" (Year 2022))
--   ,show (showDay <$> runExprM "1 wday onOrBefore 19 day  nov " (Year 2))
--   ]

runExprM :: (Typeable res, Typeable val) => Text -> val -> [([Text], res)]
runExprM q = eitherException $ runExprParser multipleFuncWithFinalListParser cals q 

cal1 = Unsafe.fromJust $ generateHolidayCalendar $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2025,1,1)]
  where toDay' (y,m,d) = toDay y m d
cals = HM.fromList [("de", cal1)]
