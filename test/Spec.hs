module Main where

import Control.Monad (unless, forM)
import Data.Char (isControl)
import Data.List (intercalate)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import JSONParser

main :: IO ()
main = do
  result <- quickCheckResult checkJSON
  unless (isSuccess result) exitFailure

checkJSON :: JValue -> Gen Property
checkJSON value = do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) = surround <$> pad (pure $ show k) <*> stringify v <*> pure ":"

instance Arbitrary JValue where
  shrink = genericShrink

  arbitrary = sized go
    where
      go n | n < 5 = frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
      go n = frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]

      scalarGens = [ pure JNull
                   , JBool   <$> arbitrary
                   , JString <$> jsonStringGen
                   , JNumber <$> arbitrary <*> listOf digits <*> arbitrary
                   ]

      compositeGens n = [ fmap JArray  . scaledListOf . go    $ n `div` 2
                        , fmap JObject . scaledListOf . objKV $ n `div` 2
                        ]

      objKV n = (,) <$> jsonStringGen <*> go n

      scaledListOf = scale (`div` 2) . listOf

      digits = choose (0, 9)

jsonStringGen :: Gen String
jsonStringGen = listOf arbitraryJSONChar
  where
    arbitraryJSONChar' =
      frequency [(9, arbitraryASCIIChar), (0, choose ('\128', '\65535'))]
        `suchThat` (\c -> not (c == '\"' || c == '\\' || isControl c))

    arbitraryJSONChar = frequency [
        (99, arbitraryJSONChar')
      , (1, elements ['"' , '\\' , '\b' , '\f' , '\n' , '\r' , '\t'])
      ]

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
  . listOf
  . elements
  $ [' ' , '\n' , '\r' , '\t']