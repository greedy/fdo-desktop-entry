{- |
Module      :   System/Freedesktop/Directories.hs
Description :   Implements freedesktop.org basedir specification
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
  Locale(..),
  DesktopEntry
) where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as M

data Key = Key String (Maybe Locale)
  deriving (Show, Eq, Ord)

type Failure = String

class Value a where
  parse :: String -> Either Failure a

newtype StringVal = StringVal String

instance Show StringVal where
  show (StringVal s) = show s

instance IsString StringVal where
  fromString = StringVal

instance Value StringVal where
  parse = Right . StringVal . unescape

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
  parse "true"  = Right True
  parse "false" = Right False
  parse x       = Left $ "Invalid boolean value: " ++ x

instance Value Float where
  parse s = case reads s of
    [(n, "")] -> Right n
    [(_, extra)] -> Left $ "Extra stuff after number"
    [] -> Left $ "No number to parse"
    
instance Value a => Value [a] where
  parse s = mapM parse $ split ';' s

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
              Just s -> case parse s of
                          Left f -> Left f
                          Right v -> Right $ Just v
