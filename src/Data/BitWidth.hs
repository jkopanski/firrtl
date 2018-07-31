{-# language
        EmptyCase
      , ScopedTypeVariables
      , TemplateHaskell #-}
module Data.BitWidth where

import Data.Singletons.Prelude
import Data.Singletons.TH
import GHC.TypeNats as TN
import Numeric.Natural (Natural)

-- ^ Runtime representation of bit width.
--   Newtype wrapper since Natural is already used by SNat
newtype Bits = Bits { unBits :: Natural }
  deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show)


-- ^ Compared to Nat bit width is at least 1
data BW = O | S BW deriving (Eq, Show, Ord)

data instance Sing (x :: BW) where
  SO :: Sing 'O
  SS :: Sing x -> Sing ('S x)

instance SingI 'O where
  sing = SO

instance SingI n => SingI ('S (n :: BW)) where
  sing = SS sing

instance SingKind BW where
  type Demote BW = Bits

  fromSing SO = Bits 1
  fromSing (SS n) = 1 + fromSing n

  toSing n = if unBits n == 1
    then SomeSing SO
    else case toSing (n - 1) of
      SomeSing sx -> SomeSing (SS sx)
