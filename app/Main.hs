module Main where

import JSONParser (parseJSON)
import Text.Pretty.Simple (pPrintNoColor)

main :: IO ()
main = do
  s <- getContents
  case parseJSON s of
    Nothing -> error "JSON parsing failed"
    Just j  -> pPrintNoColor j
