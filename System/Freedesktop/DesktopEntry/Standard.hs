{- |
Module      :   System/Freedesktop/DesktopEntry/Standard
Description :   Implements freedesktop.org basedir specification
Copyright   :   (c) Geoffrey Reedy
License     :   BSD3

Maintainer  :   geoff@programmer-monk.net
Stability   :   provisional
Portability :   portable

This module provides an interface to desktop entries as specified by the
Desktop Entry Specification at <http://www.freedesktop.org/wiki/Specifications/desktop-entry-spec>

-}

module System.Freedesktop.DesktopEntry.Standard (
  fromFile,
  fromContents,
  DesktopEntry,
  EntryType(..),
  currentLocale,
  entryType,
  version,
  name,
  genericName,
  noDisplay,
  comment,
  icon,
  hidden,
  onlyShowIn,
  notShowIn,
  tryExec,
  exec,
  path,
  terminal,
  mimeType,
  categories,
  startupNotify,
  startupWMClass,
  url
) where

import System.Freedesktop.DesktopEntry
import Text.ParserCombinators.Parsec
import System.Environment
import qualified System.IO.Error as IOE
import Control.Monad (msum)

--
-- Standard keys
--

data EntryType = Application
               | Link
               | Directory
               | Other String
  deriving (Show, Eq, Ord)

deGet :: Value a => Key -> DesktopEntry -> Either Failure (Maybe a)
deGet = get "Desktop Entry"

type NonLocalized a = DesktopEntry -> Either Failure (Maybe a)
type Localized a = Maybe Locale -> DesktopEntry -> Either Failure (Maybe a)

localized :: Value a => String -> Localized a
localized key locale = deGet (Key key locale)
nonlocalized :: Value a => String -> NonLocalized a
nonlocalized key = deGet (Key key Nothing)

str :: Either Failure (Maybe StringVal) -> Either Failure (Maybe String)
str = fmap (fmap unwrap)
strs :: Either Failure (Maybe [StringVal]) -> Either Failure (Maybe [String])
strs = fmap (fmap (fmap unwrap))

entryType :: NonLocalized EntryType
entryType = fmap (fmap convertType) . nonlocalized "Type"

(.^) a b = (\c -> a . (b c))

convertType (StringVal "Application") = Application
convertType (StringVal "Directory") = Directory
convertType (StringVal x) = Other x

version :: NonLocalized String
version = str . nonlocalized "Version"

name :: Localized String
name = str .^ localized "Name"

genericName :: Localized String
genericName = str .^ localized "GenericName"

noDisplay :: NonLocalized Bool
noDisplay = nonlocalized "NoDisplay"

comment :: Localized String
comment = str .^ localized "Comment"

icon :: Localized String
icon = str .^ localized "Icon"

hidden :: NonLocalized Bool
hidden = nonlocalized "Hidden"

onlyShowIn :: NonLocalized [String]
onlyShowIn = strs . nonlocalized "OnlyShowIn"

notShowIn :: NonLocalized [String]
notShowIn = strs . nonlocalized "NotShowIn"

tryExec :: NonLocalized String
tryExec = str . nonlocalized "TryExec"

exec :: NonLocalized String
exec = str . nonlocalized "Exec"

path :: NonLocalized String
path = str . nonlocalized "Path"

terminal :: NonLocalized Bool
terminal = nonlocalized "Terminal"

mimeType :: NonLocalized [String]
mimeType = strs . nonlocalized "MimeType"

categories :: NonLocalized [String]
categories = strs . nonlocalized "Categories"

startupNotify :: NonLocalized Bool
startupNotify = nonlocalized "StartupNotify"

startupWMClass :: NonLocalized String
startupWMClass = str . nonlocalized "StartupWMClass"

url :: NonLocalized String
url = str . nonlocalized "URL"

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv s = catch (fmap Just $ getEnv s) (\e -> if IOE.isDoesNotExistError e then return Nothing else ioError e)

currentLocale :: IO Locale
currentLocale = do
  locale <- fmap msum $ sequence (map maybeGetEnv ["LC_MESSAGES", "LC_ALL", "LANG"])
  return $ parseLocale locale

perhaps :: Parser a -> Parser (Maybe a)
perhaps p = (fmap Just p) <|> (return Nothing)

parseLocale :: Maybe String -> Locale
parseLocale Nothing = Locale "C" Nothing Nothing
parseLocale (Just s) = case parse localeParser "" s of
                         Left x -> parseLocale Nothing
                         Right l -> l

localeParser = do
  lang <- many1 alphaNum
  territory <- perhaps $ char '_' >> many1 alphaNum
  codeset <- perhaps $ char '.' >> many1 alphaNum
  modifier <- perhaps $ char '@' >> many1 alphaNum
  return $ Locale lang territory modifier
