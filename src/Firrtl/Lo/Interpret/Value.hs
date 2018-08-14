{-# language
        BangPatterns
      , GADTs
      , ScopedTypeVariables
      , TypeApplications
      , TypeInType #-}
module Firrtl.Lo.Interpret.Value where

import Data.Singletons
import Data.Singletons.Prelude.Tuple
import Firrtl.Lo.TypeCheck.Ty

-- | Runtime representation of valid values
-- Can we drop type information after typechecking?
-- I think it can be done, since FIRRTL types make
-- sure that operation results can fit into type widths
type Value = Int

lowerBound, upperBound :: forall (t :: Ty). Sing t -> Value
lowerBound s = case s of
    STuple3 sign wid _ ->
      let n :: Int = fromIntegral $ fromSing wid
          maxUIntVal :: Int = 2 ^ n - 1
       in case sign of
            SClock    -> 0
            SSigned   -> negate (floor (fromIntegral maxUIntVal / 2 :: Float))
            SUnsigned -> 0

upperBound s = case s of
    STuple3 sign wid _ ->
      let n :: Int = fromIntegral $ fromSing wid
          maxUIntVal :: Int = 2 ^ n - 1
       in case sign of
            SClock    -> 1
            SSigned   -> ceiling (fromIntegral maxUIntVal / 2 :: Float)
            SUnsigned -> maxUIntVal

