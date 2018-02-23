module Main where

import Control.Monad (unless)
import Data.Char (isControl, isAscii, isPrint)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import JSONParser

main :: IO ()
main = do
  result <- verboseCheckWithResult (stdArgs { maxSize = 40 }) $ \j -> parseJSON (show j) == Just j
  unless (isSuccess result) exitFailure

instance Arbitrary JValue where
  arbitrary = sized go
    where
      scalarGens = [ pure JNull
                   , JBool   <$> arbitrary
                   , JString <$> arbitraryJSONString
                   , JNumber <$> arbitrary <*> oneof [choose (-5, 0), arbitrary]
                   ]
      compositeGens n = [ fmap JArray . scale scaleComp . listOf . go $ n `div` 2
                        , fmap JObject . scale scaleComp . listOf $ ((,) <$> arbitraryJSONString <*> go (n `div` 2))
                        ]

      scaleComp n = n * 2 `div` 3

      go 0 = oneof scalarGens
      go n = frequency [(2, oneof scalarGens), (3, oneof (compositeGens n))]

      arbitraryJSONString = listOf arbitraryJSONChar'

      arbitraryJSONChar' =
        arbitraryUnicodeChar `suchThat` (\c -> isAscii c && isPrint c && not (c == '\"' || c == '\\' || isControl c))

      arbitraryJSONChar = frequency [
          (99, arbitraryJSONChar')
        , (1, elements ['"' , '\\' , '\b' , '\f' , '\n' , '\r' , '\t'])
        ]

  shrink = genericShrink
