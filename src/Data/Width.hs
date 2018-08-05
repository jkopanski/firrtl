{-# language
        EmptyCase
      , ScopedTypeVariables
      , TemplateHaskell #-}
module Data.Width where

import Data.Singletons.Prelude
import Data.Singletons.TH
import GHC.TypeNats (Nat)
import Numeric.Natural (Natural)
import Unsafe.Coerce

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

type family BWPlus (a :: BW) (b :: BW) :: BW where
  BWPlus 'O b = 'S b
  BWPlus ('S a) b = 'S (BWPlus a b)

type family BWMul (a :: BW) (b :: BW) :: BW where
  BWMul 'O b = b
  BWMul ('S a) b = BWPlus b (BWMul a b)

type family BWMinus (a :: BW) (b :: BW) :: BW where
  BWMinus 'O _ = 'O
  BWMinus ('S a) ('S b) = BWMinus a b
  BWMinus ('S a) 'O = a

type family BWAbs (a :: BW) :: BW where
  BWAbs a = a

type family BWSignum (a :: BW) :: BW where
  BWSignum _ = 'O

type family BWFromInteger (a :: Nat) :: BW where
  BWFromInteger 1 = 'O
  BWFromInteger n = 'S (BWFromInteger (n - 1))

instance PNum BW where
  type a + b = BWPlus a b
  type a - b = BWMinus a b
  type a * b = BWMul a b
  type Negate (a :: BW) = Error "Cannot negate bit width"
  type Abs a = BWAbs a
  type Signum a = BWSignum a
  type FromInteger a = BWFromInteger a

instance SNum BW where
  sa %+ sb =
    let a = fromSing sa
        b = fromSing sb
        ex = toSing (a + b)
    in
    case ex of
      SomeSing w -> unsafeCoerce w

  sa %- sb =
    let a = fromSing sa
        b = fromSing sb
        ex = toSing (a - b)
    in
    case ex of
      SomeSing w -> unsafeCoerce w

  sa %* sb =
    let a = fromSing sa
        b = fromSing sb
        ex = toSing (a * b)
    in
    case ex of
      SomeSing w -> unsafeCoerce w

  sNegate _ = error "Cannot call sNegate on a bit width singleton."

  sAbs x = x

  sSignum _ = SO

  sFromInteger sn =
    case sn %~ (sing :: Sing 1) of
      Proved Refl -> SO
      Disproved _ -> unsafeCoerce $
        SS (sFromInteger $ sn %- (sing :: Sing 1))

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
