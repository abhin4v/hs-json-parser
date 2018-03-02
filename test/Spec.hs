module Main where

import Control.Monad (unless)
import Data.Char (isControl)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import JSONParser

main :: IO ()
main = do
  result <- quickCheckResult $ \j -> parseJSON (show j) == Just j
  unless (isSuccess result) exitFailure

instance Arbitrary JValue where
  shrink = genericShrink

  arbitrary = sized go
    where
      go 0 = oneof scalarGens
      go n = frequency [(2, oneof scalarGens), (3, oneof (compositeGens n))]

      scalarGens = [ pure JNull
                   , JBool   <$> arbitrary
                   , JString <$> arbitraryJSONString
                   , JNumber <$> arbitrary <*> oneof [choose (-5, 0), arbitrary]
                   ]

      compositeGens n = [ fmap JArray  . scaledListOf . go    $ n `div` 2
                        , fmap JObject . scaledListOf . objKV $ n `div` 2
                        ]

      objKV n = (,) <$> arbitraryJSONString <*> go n

      scaledListOf = scale (\n -> n * 2 `div` 3) . listOf

arbitraryJSONString :: Gen String
arbitraryJSONString = listOf arbitraryJSONChar
  where
    arbitraryJSONChar' =
      frequency [(9, arbitraryASCIIChar), (0, choose ('\128', '\65535'))]
        `suchThat` (\c -> not (c == '\"' || c == '\\' || isControl c))

    arbitraryJSONChar = frequency [
        (99, arbitraryJSONChar')
      , (1, elements ['"' , '\\' , '\b' , '\f' , '\n' , '\r' , '\t'])
      ]
