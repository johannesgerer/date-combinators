
{-# LANGUAGE NoMonomorphismRestriction #-}
module DateCombinators.Parser
  (module DateCombinators.Parser
  ,module DateCombinators) where

import           Chronos hiding (second, may, day)
import           Data.Dynamic
import qualified Data.Maybe as Unsafe
import           DateCombinators
import           Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Number as N
import           Type.Reflection
import           Type.Reflection as R
import           Yahp hiding (optional, many, try, typeRep)
-- import           DateCombinators.BusinessDay

data DynFunc = DynFunc { dName :: Text
                       , dFunc :: Dynamic
                       }

toDynF :: Typeable a => Text -> a -> DynFunc
toDynF n = DynFunc n . toDyn

type P a b = Parsec Text () (a -> Reader HolidayCalendars b)
type PD a = Parsec Text () (Reader HolidayCalendars a)

exprParser :: HasCallStack => PD DynFunc -> P Int DynFunc -> P (String, Int) DynFunc -> PD DynFunc
exprParser o1 o2 o3 = (<* spaces) $ choice $ fmap try 
  [(\i f s -> f (s, i)) <$> N.int <*> (spaces >> o3) <*> mP
  ,(\i f -> f i) <$> N.int <*> (spaces >> o2)
  ,o1]
  where mP = spaces >> char '@' >> (:) <$> letter <*> many (noneOf " ,]")

o1Default :: PD DynFunc
o1Default = choice $ fmap (\(n,d) -> pure (DynFunc (toS n) d) <$ try (string n)) $
  fmap (second toDyn) [("month"         , month)
                      ,("before"        , before)
                      ,("after"         , after)
                      ,("onOrBefore"    , onOrBefore)
                      ,("onOrAfter"     , onOrAfter)
                      ]
  <> fmap (second $ toDyn)         [( "jan",  jan)
                                        ,( "feb",  feb)
                                        ,( "mar",  mar)
                                        ,( "apr",  apr)
                                        ,( "may",  may)
                                        ,( "jun",  jun)
                                        ,( "jul",  jul)
                                        ,( "aug",  aug)
                                        ,( "sep",  sep)
                                        ,( "oct",  oct)
                                        ,( "nov",  nov)
                                        ,( "dec",  dec)
                                        ,( "q1",   q1)
                                        ,( "q2",   q2)
                                        ,( "q3",   q3)
                                        ,( "q4",   q4)
                                        ]
  -- <> [("everyMonth", toDyn $ fmap2 toDyn everyMonth)
     -- ,("everyQuarter", toDyn $ fmap2 toDyn everyQuarter)] 

o2Default :: P Int DynFunc
o2Default = choice $ fmap (\(b,a) -> (\i -> pure (DynFunc (toS b) $ a i)) <$ try (string b)) $
  fmap (second $ fmap toDyn) [("monthsAfter"    , addGregorianMonths)
                             ,("monthsBefore"   , addGregorianMonths . negate)
                             ,("daysAfter"      , addDays)
                             ,("daysBefore"     , addDays . negate)]
  <> 
  fmap (second $ fmap toDyn) [("wday", weekday)
                             ,( "mon",  mon)
                             ,( "day",  day)
                             ,( "tue",  tue)
                             ,( "wed",  wed)
                             ,( "thu",  thu)
                             ,( "fri",  fri)
                             ,( "sat",  sat)
                             ,( "sun",  sun)
                             ]

o3Default :: HasCallStack => P (String, Int) DynFunc
o3Default = (\(s,i) -> reader $ \cals -> toDynF "bday" $ businessDay cals (toS s) i) <$ string "bday"

singleFuncParser :: HasCallStack => PD DynFunc
singleFuncParser = exprParser o1Default o2Default o3Default 

foldApply :: HasCallStack => [DynFunc] -> Dynamic -> Dynamic
foldApply = flip (foldl' $ flip dynFApp) . reverse

applyDynamicFunctions :: forall res . (Typeable res, HasCallStack) => [DynFunc] -> Dynamic -> res
applyDynamicFunctions funs = Unsafe.fromJust . fromDynamic . foldApply (toDynF "final step" (id :: res -> res) : funs)


multipleFuncParser :: (Typeable val, Typeable res, HasCallStack) => PD (val -> res)
multipleFuncParser  = (fmap ((. toDyn) . applyDynamicFunctions) . sequence) <$> (spaces *> (many1 singleFuncParser) <* eof)


multipleFuncWithFinalListParser :: forall val res . (Typeable res, Typeable val) => PD (val -> [([Text], res)])
multipleFuncWithFinalListParser = do
  (funcs :: [Reader HolidayCalendars DynFunc]) <- spaces *> (many singleFuncParser)
  (terminal :: [[Reader HolidayCalendars DynFunc]]) <- ([[]] <$ eof)
    <|> char '[' *> ((spaces *> many1 singleFuncParser) `sepBy1` char ',') <* (char ']' >> spaces)
  pure $ fmap (mapM combine) $ mapM (sequence . (funcs <>)) terminal
  -- pure $ combine <$> sequence funcs <*> sequence terminal
  where combine :: [DynFunc] -> val -> ([Text], res)
        combine fs val = (dName <$> fs, applyDynamicFunctions fs $ toDyn val)

multipleFuncWithFinalListParserExec :: (Typeable res, Typeable val) => PD (val -> [res])
multipleFuncWithFinalListParserExec = (fmap3 . fmap) snd multipleFuncWithFinalListParser

runExprParser :: MonadError Text m => PD a -> HolidayCalendars -> Text -> m a
runExprParser p cals = either (throwError . toS . show) (pure . flip runReader cals) . runParser p () ""

dynListSequence :: Dynamic -> [Dynamic]
dynListSequence x | Dynamic t v <- x
                  , Just HRefl <- t `eqTypeRep` R.typeRep @[Dynamic] =  v
                  | True = [x]

dynFApp :: DynFunc -> Dynamic -> Dynamic
dynFApp (DynFunc _ (Dynamic (Fun ta tr) f)) (Dynamic ta' x)
  | Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Dynamic tr (f x)
dynFApp (DynFunc nf f) v
  = throw $ DynamicTypeMismatch $ "Type error in dynamic application.\n" ++
    "Can't apply function '" <> toS nf <> "' " ++ show f ++ " to argument " ++ show v

-- * public functions

runMultFunc :: (MonadError Text m, Typeable res, Typeable val) => (res -> Day) -> HolidayCalendars -> Text -> val -> m [([Text], res)]
runMultFunc f cals input = (=<< runExprParser multipleFuncWithFinalListParser cals input)
  . (\val -> catchFoldDayError (f . snd) . ($ val))

runFunc :: (MonadError Text m, Typeable a, Typeable b) => (b -> Day) -> HolidayCalendars -> Text -> a -> m b
runFunc f cals input = (=<< runExprParser multipleFuncParser cals input)
  . (\val -> catchDayError f . ($ val))

execMultFunc :: (MonadError Text f, Typeable val, Typeable res) => (res -> Day) -> HolidayCalendars -> Text -> val -> f [res]
execMultFunc f cals input = fmap2 snd . runMultFunc f cals input

