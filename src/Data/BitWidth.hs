{-# language
        EmptyCase
      , ScopedTypeVariables
      , TemplateHaskell #-}
module Data.BitWidth where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Numeric.Natural (Natural)

-- ^ Runtime representation of bit width.
--   Newtype wrapper since Natural is already used by SNat
newtype Width = Width { unWidth :: Natural }
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
  type Demote BW = Width

  fromSing SO = Width 1
  fromSing (SS n) = 1 + fromSing n

  toSing n = if unWidth n == 1
    then SomeSing SO
    else case toSing (n - 1) of
      SomeSing sx -> SomeSing (SS sx)

$(genPromotions [''BW])
$(singEqInstance ''BW)
$(singDecideInstance ''BW)

-- $(singletons [d|
--   bwPlus :: BW -> BW -> BW
--   bwPlus O b = S b
--   bwPlus (S a) b = S (bwPlus a b)

--   bwMul :: BW -> BW -> BW
--   bwMul O b = b
--   bwMul (S a) b = bwPlus b (bwMul a b)

--   bwMinus :: BW -> BW -> BW
--   bwMinus O _ = O
--   bwMinus (S a) (S b) = bwMinus a b
--   bwMinus (S a) O = a

--   bwAbs :: BW -> BW
--   bwAbs n = n

--   bwSignum :: BW -> BW
--   bwSignum _ = O

--   instance Num BW where
--     (+) = bwPlus
--     (-) = bwMinus
--     (*) = bwMul
--     abs = bwAbs
--     signum = bwSignum
--     fromInteger n
--       = if n == 1
--            then O
--            else S (fromInteger (n - 1))
--   |])
