{- |
Module      :   System/Freedesktop/DesktopEntry.hs
Description :   Implements freedesktop.org desktop-entry specification
Copyright   :   (c) Geoffrey Reedy
License     :   BSD3

Maintainer  :   geoff@programmer-monk.net
Stability   :   provisional
Portability :   portable

This module provides an interface to desktop entries as specified by the
Desktop Entry Specification at <http://www.freedesktop.org/wiki/Specifications/desktop-entry-spec>

-}

module System.Freedesktop.DesktopEntry (
  Key (..),
  Failure,
  Value,
  StringVal(..),
  unwrap,
  Locale(..),
  DesktopEntry,
  get,
  fromContents,
  fromFile
) where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad.Error
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Key = Key String (Maybe Locale)
  deriving (Show, Eq, Ord)

type Failure = String

class Value a where
  convert :: String -> Either Failure a

newtype StringVal = StringVal String

unwrap (StringVal s) = s

instance Show StringVal where
  show (StringVal s) = show s

instance IsString StringVal where
  fromString = StringVal

instance Value StringVal where
  convert = Right . StringVal . unescape

unescape :: String -> String
unescape ('\\':'s':xs)  = ' ':(unescape xs)
unescape ('\\':'n':xs)  = '\n':(unescape xs)
unescape ('\\':'t':xs)  = '\t':(unescape xs)
unescape ('\\':'r':xs)  = '\r':(unescape xs)
unescape ('\\':'\\':xs) = '\\':(unescape xs)
unescape ('\\':x:xs)    = x:(unescape xs)
unescape (x:xs)         = x:(unescape xs)
unescape []             = []

instance Value Bool where
  convert "true"  = Right True
  convert "false" = Right False
  convert x       = Left $ "Invalid boolean value: " ++ x

instance Value Float where
  convert s = case reads s of
    [(n, "")] -> Right n
    [(_, extra)] -> Left $ "Extra stuff after number"
    [] -> Left $ "No number to parse"

instance Value a => Value [a] where
  convert s = mapM convert $ split ';' s

split :: Char -> String -> [String]
split c s = unfoldr (split1' c) s
  where split1' c s = case split1 c s of
                        ("", "") -> Nothing
                        x -> Just x

split1 :: Char -> String -> (String, String)
split1 c xs = case xs of
  "" -> ("","")
  ('\\':x:xs) | x == c -> let (a,b) = split1 c xs in (c:a,b)
  (x:xs) | x == c    -> ("", xs)
         | otherwise -> let (a,b) = split1 c xs in (x:a,b)

data Locale = Locale {
  lang :: String,
  country :: Maybe String,
  modifier :: Maybe String
} deriving (Show, Eq, Ord)

newtype DesktopEntry = DesktopEntry (M.Map String Group)
type Group = M.Map String Values
type Values = M.Map (Maybe Locale) String

getGroup :: String -> DesktopEntry -> Maybe Group
getGroup gn (DesktopEntry de) = M.lookup gn de

keys :: Maybe Locale -> [Maybe Locale]
keys (Just (Locale lang (Just country) (Just modifier))) =
  [Just $ Locale lang (Just country) (Just modifier),
   Just $ Locale lang (Just country) Nothing,
   Just $ Locale lang Nothing (Just modifier),
   Just $ Locale lang Nothing Nothing,
   Nothing]
keys (Just (Locale lang (Just country) Nothing)) =
  [Just $ Locale lang (Just country) Nothing,
   Just $ Locale lang Nothing Nothing,
   Nothing]
keys (Just (Locale lang Nothing (Just modifier))) =
  [Just $ Locale lang Nothing (Just modifier),
   Just $ Locale lang Nothing Nothing,
   Nothing]
keys (Just (Locale lang Nothing Nothing)) =
  [Just $ Locale lang Nothing Nothing,
   Nothing]
keys Nothing = [Nothing]

getValue :: Key -> Group -> Maybe String
getValue (Key name locale) group = do
  values <- M.lookup name group
  listToMaybe $ mapMaybe (flip M.lookup values) (keys locale)

get :: Value a => String -> Key -> DesktopEntry -> Either Failure (Maybe a)
get g k e = case (getGroup g e) >>= (getValue k) of
              Nothing -> Right Nothing
              Just s -> case convert s of
                          Left f -> Left f
                          Right v -> Right $ Just v

--
-- Parsing
--

fromContents = parse file ""

fromFile = parseFromFile file

blankLine = manyTill inlineSpace newline
comment = char '#' >> manyTill anyChar (try newline)
blankLines = skipMany (blankLine <|> comment)

inlineSpace = oneOf " \t"

-- group names may contain all ASCII characters except for '[' and ']' and control characters
groupNameChars = '\\' : [' '..'Z'] ++ ['^'..'~']

-- only the characters A-Za-z0-9- may be used in key names
keyNameChars = '-' : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

file :: Parser DesktopEntry
file = do
  blankLines
  groups <- many aGroup
  eof
  return $ DesktopEntry $ M.fromList groups

aGroup :: Parser (String, Group)
aGroup = do
  name <- (between (char '[') (char ']') groupName)
  blankLines
  entries <- many (entry)
  return (name, reduceEntries entries)

reduceEntries :: [(String, Maybe Locale, String)] -> Group
reduceEntries [] = M.empty
reduceEntries ((name, locale, value):es) =
  M.alter (Just . M.insert locale value . fromMaybe M.empty) name (reduceEntries es)


groupName = many1 (oneOf groupNameChars)

entry :: Parser (String, Maybe Locale, String)
entry = do
  name <- many1 (oneOf keyNameChars)
  locale <- option Nothing localeString
  many inlineSpace
  char '='
  many inlineSpace
  value <- manyTill anyChar newline
  return (name, locale, value)

perhaps :: Parser a -> Parser (Maybe a)
perhaps p = (fmap Just p) <|> (return Nothing)

localeString :: Parser (Maybe Locale)
localeString = do
  char '['
  lang <- many1 letter
  country <- perhaps $ char '_' >> (many1 letter)
  modifier <- perhaps $ char '@' >> (many1 letter)
  char ']'
  return $ Just $ Locale lang country modifier


