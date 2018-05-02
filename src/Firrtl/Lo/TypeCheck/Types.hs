{-|
This module contains valid lowered FIRRTL types,
which are basically Ground types:
- Unsigned Integer,
- Signed Integer,
- Clock.

Lowered form mandates explicit width.

Although it is mentioned only later in spec,
type should contain gender as well
|-}
module Firrtl.Lo.TypeCheck.Types
  ( Gender (..)
  , Type (..)
  , ConnType (..)
  , bi
  , female
  , male
  , ground
  , width
  -- connection helpers
  , connectable
  , containable
  , equivalent
  , minBitWidth
  , minSignedBitWidth
  ) where

import qualified Numeric.Natural as N
data Gender
  = Bi
  | Female
  | Male
  deriving (Eq, Show)

data Type
  = Natural N.Natural -- | @Nat@ has it's value in the type level, not it's width
  | Unsigned Int
  | Signed Int
  | Clock
  deriving (Eq, Show)

-- | Type with gender information
data ConnType = ConnType
 { connGender :: Gender
 , connType   :: Type
 } deriving (Eq, Show)

bi, female, male :: Type -> ConnType
male = ConnType Male
female = ConnType Female
bi = ConnType Bi

class HasWidth t where
  width :: t -> Int

instance HasWidth Type where
  width (Unsigned w) = w
  width (Signed w) = w
  width Clock    = 1
  width (Natural n)  = minBitWidth $ fromIntegral $ toInteger n

instance HasWidth ConnType where
  width = width . connType

class HasGround t where
  ground :: t -> Type

instance HasGround Type where
  ground = id

instance HasGround ConnType where
  ground = connType

equivalent :: HasGround t => t -> t -> Bool
equivalent lhs rhs = equiv (ground lhs) (ground rhs)
  where
    equiv :: Type -> Type -> Bool
    equiv (Unsigned _) (Unsigned _) = True
    equiv (Signed   _) (Signed   _) = True
    equiv Clock         Clock       = True
    equiv (Natural  _) (Natural  _) = True
    equiv _ _ = False

containable :: HasWidth t => t -> t -> Bool
containable lhs rhs = width lhs >= width rhs

connectable :: ConnType -> ConnType -> Bool
connectable lhs rhs =
  not $ (connGender lhs == Male) || (connGender rhs == Female)

minBitWidth :: Int -> Int
minBitWidth = (+) 1
            . (floor :: Double -> Int)
            . logBase 2
            . fromIntegral

minSignedBitWidth :: Int -> Int
minSignedBitWidth x | x > 0 = 1 + minBitWidth x
                    | otherwise = ( (+) 1
                                  . (ceiling :: Double -> Int)
                                  . logBase 2
                                  . abs
                                  . fromIntegral
                                  ) x
