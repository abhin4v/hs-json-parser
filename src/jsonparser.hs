{-# LANGUAGE DeriveGeneric #-}
module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Char (isDigit, isHexDigit, isSpace, isControl, chr, digitToInt)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)

newtype Parser i o = Parser { runParser :: i -> Maybe (i, o) }

instance Functor (Parser i) where
  fmap f p = Parser $ fmap (fmap f) . runParser p

instance Applicative (Parser i) where
  pure x = Parser $ pure . (\a -> (a, x))
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = Parser $ \i -> case i of
  (x:xs) | p x -> Just (xs, x)
  _            -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

hexDigit :: Parser String Int
hexDigit = digitToInt <$> satisfy isHexDigit

string :: String -> Parser String String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

space :: Parser String Char
space = satisfy isSpace

digits :: Parser String [Int]
digits = some digit

spaces :: Parser String String
spaces = many space

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s = (:) <$> v <*> many (s *> v) <|> pure []

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { significand :: Integer, exponent :: Integer}
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)

instance Show JValue where
  show value = case value of
    JNull       -> "null"
    JBool True  -> "true"
    JBool False -> "false"
    JString s   -> "\"" ++ s ++ "\""
    JNumber s e -> case e of
      0 -> show s
      _ | e >= (-5) && e < 0 -> printf ("%." ++ show (abs e) ++ "f") (toDouble s e)
      _ -> show s ++ "e" ++ show e
    JArray a    -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o   -> "{" ++ intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) o) ++ "}"
    where
      toDouble :: Integer -> Integer -> Double
      toDouble s e = fromInteger s * 10 ^^ e

jNull :: Parser String JValue
jNull = string "null" $> JNull

jBool :: Parser String JValue
jBool =   string "true"  $> JBool True
      <|> string "false" $> JBool False

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base = foldl (\num d -> num * fromIntegral base + fromIntegral d)

jString :: Parser String JValue
jString = JString <$> (char '"' *> many jsonChar <* char '"')
  where
    jsonChar =   satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
             <|> string "\\\"" $> '"'
             <|> string "\\\\" $> '\\'
             <|> string "\\/"  $> '/'
             <|> string "\\b"  $> '\b'
             <|> string "\\f"  $> '\f'
             <|> string "\\n"  $> '\n'
             <|> string "\\r"  $> '\r'
             <|> string "\\t"  $> '\t'
             <|> chr . fromIntegral . digitsToNumber 16 0 <$> (string "\\u" *> replicateM 4 hexDigit)

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

jUInt :: Parser String Integer
jUInt =   (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits
      <|> fromIntegral <$> digit

data Sign = Positive | Negative

jInt' :: Parser String (Sign, Integer)
jInt' = mkInt <$> optional (char '-') <*> jUInt
  where
    mkInt (Just '-') i = (Negative, i)
    mkInt _          i = (Positive, i)

applySign :: (Sign, Integer) -> Integer
applySign (Negative, i) = negate i
applySign (Positive, i) = i

jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E') *> (mkExp <$> optional (char '+' <|> char '-') <*> jUInt)
  where
    mkExp (Just '-') i = negate i
    mkExp _ i          = i

jInt :: Parser String JValue
jInt = JNumber <$> (applySign <$> jInt') <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> (applySign <$> jInt') <*> jExp

jIntFrac :: Parser String JValue
jIntFrac =
  (\(sign, i) f -> JNumber (applySign (sign, digitsToNumber 10 i f)) (fromIntegral . negate . length $ f))
  <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ ~(JNumber i e) e' -> JNumber i (e + e')) <$> jIntFrac <*> jExp

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

jArray :: Parser String JValue
jArray = JArray <$> (char '[' *> (jValue `separatedBy` char ',' `surroundedBy` spaces) <* char ']')

jObject :: Parser String JValue
jObject = JObject <$> (char '{' *> pair `separatedBy` char ',' <* char '}')
  where
    pair = (\ ~(JString s) j -> (s, j)) <$> (jString `surroundedBy` spaces) <* char ':' <*> jValue

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =   jNull
            <|> jBool
            <|> jString
            <|> jNumber
            <|> jArray
            <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", j) -> Just j
  _            -> Nothing
