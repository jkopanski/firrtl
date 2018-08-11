{-# language
        BangPatterns
      , GADTs
      , ScopedTypeVariables
      , TypeApplications
      , TypeInType #-}
module Firrtl.Lo.Interpret.Value where

import Data.Kind (type (*))
import Data.Singletons
import Data.Singletons.Prelude.Tuple
import Data.Width
import Firrtl.Lo.TypeCheck.Ty

-- TODO: make Validity a HKT and write down some typial instances
data Value :: Ty -> * where
  Invalid :: Value t
  Valid   :: Sing t -> !Int -> Value t

instance Eq (Value t) where
  Invalid == _ = False
  _ == Invalid = False
  (==) (Valid _ vl) (Valid _ vr) = vl == vr

instance Show (Value t) where
  show Invalid = "Invalid"
  show (Valid s i) = "Valid " <> show (fromSing s) <> " " <> show i

instance Ord (Value t) where
  compare Invalid _ = LT
  compare _ Invalid = GT
  compare (Valid _ vl) (Valid _ vr) = compare vl vr

instance forall (t :: Ty). SingI t => Bounded (Value t) where
  maxBound = case sing :: Sing t of
    s@(STuple3 sign wid _) ->
      let
        n :: Int = fromIntegral $ fromSing wid
        maxUIntVal :: Int = 2 ^ n - 1 
      in
        Valid s $ case sign of
          SClock    -> 1
          SSigned   -> ceiling (fromIntegral maxUIntVal / 2)
          SUnsigned -> maxUIntVal

  minBound = case sing :: Sing t of
    s@(STuple3 sign wid _) ->
      let
        n :: Int = fromIntegral $ fromSing wid
        maxUIntVal :: Int = 2 ^ n - 1 
      in
        Valid s $ case sign of
          SClock    -> 0
          SSigned   -> negate (floor (fromIntegral maxUIntVal / 2))
          SUnsigned -> 0

zero :: Value '( 'Unsigned, Lit 1, 'Male)
zero = Valid (STuple3 SUnsigned SO SMale) 0

one :: Value '( 'Unsigned, Lit 1, 'Male)
one = Valid (STuple3 SUnsigned SO SMale) 1

