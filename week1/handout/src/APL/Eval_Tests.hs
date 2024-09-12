module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCstInt, testAdd, testSub, testMul, testDiv, testPow]

testCstInt :: TestTree
testCstInt = testCase "Testing constant integers" $
  eval (CstInt 5) @?= ValInt 5

testAdd :: TestTree
testAdd = testCase "Testing addition" $
  eval (Add (CstInt 5) (CstInt 5)) @?= ValInt 10

testSub :: TestTree
testSub = testCase "Testing subtraction" $
  eval (Sub (CstInt 5) (CstInt 5)) @?= ValInt 0

testMul :: TestTree
testMul = testCase "Testing multiplication" $
  eval (Mul (CstInt 5) (CstInt 5)) @?= ValInt 25

testDiv :: TestTree
testDiv = testCase "Testing division of 2 positive numbers" $
  eval (Div (CstInt 5) (CstInt 5)) @?= ValInt 1

testPow :: TestTree
testPow = testCase "Testing squaring" $
  eval (Pow (CstInt 2) (CstInt 3)) @?= ValInt 8