{-# language
        TypeApplications
      , TypeInType #-}
module Interpret.Eval where

import Data.Width
import Data.Singletons.Prelude
import Numeric.Natural

import           Firrtl.Lo.Context
import qualified Firrtl.Lo.Syntax.Safe.Expr as SE
import           Firrtl.Lo.Interpret.Eval
import           Firrtl.Lo.Interpret.Value
import           Firrtl.Lo.TypeCheck.Ty

import Test.Tasty
import Test.Tasty.HUnit

-- | some predefined constants
bit :: Sing '( 'Unsigned, Lit 1, 'Male)
bit   = STuple3 SUnsigned SO SMale
false = SE.mkUInt bit 0
true  = SE.mkUInt bit 1
invalidCond = SE.mkValid bit false true

zero, one', two, three, four :: Maybe Value -- '( 'Unsigned, Lit 4, 'Male)
zero  = Just 0
one'  = Just 1
two   = Just 2
three = Just 3
four  = Just 4

bw4UInt = STuple3 SUnsigned (sLit @4) SMale
exprUInt :: Natural -> SE.Expr '( 'Unsigned, Lit 4, 'Male)
exprUInt n = SE.mkUInt bw4UInt n

mone, mtwo, mthree :: Maybe Value -- '( 'Signed, Lit 4, 'Male)
mone   = Just (-1)
mtwo   = Just (-2)
mthree = Just (-3)

bw4SInt = STuple3 SSigned (sLit @4) SMale
exprSInt :: Int -> SE.Expr '( 'Signed, Lit 4, 'Male)
exprSInt i = SE.mkSInt bw4SInt i

ctx :: Context Int
ctx = insert "c" (-3)
    $ insert "b" 0
    $ singleton "a" 4

evalExprTests :: TestTree
evalExprTests = testGroup "expression evaluator tests"
  [ testGroup "constants"
    [ testCase "unsigned int" $
        eval ctx (exprUInt 4) @?= four 
    , testCase "signed int" $
        eval ctx (exprSInt (-3)) @?= mthree
    ]
  , testGroup "references"
    [ testCase "usigned" $
        eval ctx (SE.mkRef bw4UInt "a") @?= four
    , testCase "zero" $
        eval ctx (SE.mkRef (STuple3 SUnsigned SO SMale) "b") @?= zero
    , testCase "signed" $
        eval ctx (SE.mkRef bw4SInt "c") @?= mthree
    ]
  , testGroup "conditional validity"
    [ testCase "all valid, condition true" $
        eval ctx (SE.mkValid
                   bw4UInt
                   true
                   (exprUInt 2)) @?= two
    , testCase "all valid, condition false" $
        assertBool "produces valid output" $
                   eval ctx
                     (SE.mkValid
                       bw4UInt
                       false
                       (exprUInt 2)) /= two
    , testCase "invalid condition" $
        assertBool "produces valid output" $
          eval ctx
            (SE.mkValid
              bw4UInt
              invalidCond
              (exprUInt 2)) /= two
    , testCase "valid condition, invalid signal" $
        assertBool "produces valid output" $
          eval ctx
            (SE.mkValid
              bw4UInt
              true
              (SE.mkValid bw4UInt false (exprUInt 2))) /= two
    ]
  , testGroup "multiplexor"
    [ testCase "all valid, cond 0" $
        eval ctx (SE.mkMux
                   bw4SInt
                   false
                   (exprSInt (-1))
                   (exprSInt (-3))) @?= mthree
    , testCase "all valid, cond 1" $
        eval ctx (SE.mkMux
                   bw4SInt
                   true
                   (exprSInt (-1))
                   (exprSInt (-3))) @?= mone
    , testCase "invalid cond" $
        assertBool "produces valid output" $
          eval ctx
            (SE.mkMux
              bw4SInt
              invalidCond
              (exprSInt (-1))
              (exprSInt (-3))) /= mone
    , testCase "valid cond, invalid sig" $
        eval ctx
          (SE.mkMux
            bw4SInt
            false
            (SE.mkValid bw4SInt false (exprSInt (-1)))
            (exprSInt (-3))) @?= mthree
    , testCase "valid cond, invalid sig" $
        assertBool "produces valid output" $
          eval ctx
            (SE.mkMux
               bw4SInt
               true
               (SE.mkValid bw4SInt false (exprSInt (-1)))
               (exprSInt (-3))) /= mone
    ]
  , testGroup "add"
    [ testCase "both UInt max" $
        eval ctx
          (SE.mkAdd
            (STuple3 SUnsigned (sLit @5) SMale)
            (exprUInt (15))
            (exprUInt (15)))
          @?= Just 30
    , testCase "UInt max, SInt min" $
        eval ctx
          (SE.mkAdd
            (STuple3 SSigned (sLit @6) SMale)
            (exprUInt (8))
            (exprSInt (-8)))
          @?= zero
    , testCase "SInt max, UInt max" $
        eval ctx
          (SE.mkAdd
            (STuple3 SSigned (sLit @6) SMale)
            (exprSInt (7))
            (exprUInt (15)))
          @?= Just 22
    , testCase "SInt max, SInt min" $
        eval ctx
          (SE.mkAdd
            (STuple3 SSigned (sLit @5) SMale)
            (exprSInt (7))
            (exprSInt (-8)))
          @?= Just (-1)
    ]
  ]
