{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module ScrapeItchIOBundle
  ( main
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.ByteString.Char8 (ByteString)
import Data.Csv (DefaultOrdered, ToNamedRecord, encodeDefaultOrderedByName)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.HTTP.Client (managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hCookie)
import Prelude
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.HTML.Scalpel
  ( Config(..), (//), (@:), (@=), Scraper, URL, chroots, defaultDecoder, hasClass, matches
  , scrapeURLWithConfig, text
  )
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy

data Game = Game
  { name :: String
  , win :: Char
  , linux :: Char
  , mac :: Char
  , author :: String
  , description :: Maybe String
  } deriving stock (Generic, Show)
    deriving anyclass (DefaultOrdered, ToNamedRecord)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> hPutStrLn stderr "Cookie header value must be provided!"
    [cookieHeaderValue] -> run $ ByteString.pack cookieHeaderValue
    _args -> hPutStrLn stderr "Too many arguments provided!"

run :: ByteString -> IO ()
run cookieHeaderValue = do
  httpManager <- newManager tlsManagerSettings
    { managerModifyRequest = \initReq -> do
        req <- managerModifyRequest tlsManagerSettings initReq
        pure req
          { requestHeaders = (hCookie, cookieHeaderValue) : requestHeaders req
          }
    }

  games <- do
    fmap (mconcat . catMaybes) do
      for [1..34] \i -> do
        scrapeURLWithConfig (mkScalpelCfg httpManager) (mkUrl i) gamesScraper

  ByteString.Lazy.writeFile "bundle-for-ukraine.csv" $ encodeDefaultOrderedByName games
  where
  mkScalpelCfg httpManager = Config { decoder = defaultDecoder, manager = Just httpManager }

  mkUrl :: Int -> URL
  mkUrl pageNumber =
    "https://itch.io/bundle/download/SJ1IK3HdEDflhiNeOSJYnMNwDVSRNwCeKLy3CMou?page="
      <> show pageNumber

  gamesScraper :: Scraper String [Game]
  gamesScraper = chroots ("div" @: [hasClass "game_row_data"]) game
    where
    game :: Scraper String Game
    game = do
      name <- text $ ("h2" @: [hasClass "game_title"])
      win <- osSupport "Available for Windows"
      linux <- osSupport "Available for Linux"
      mac <- osSupport "Available for macOS"
      description <- do
        shortText <- text $ ("div" @: [hasClass "game_short_text"])
        if null shortText then do
          pure Nothing
        else do
          pure $ Just shortText
      author <- text $ ("div" @: [hasClass "game_author"] // "a")
      pure Game
        { name
        , win
        , linux
        , mac
        , author
        , description
        }

  osSupport :: String -> Scraper String Char
  osSupport titleAttrValue =
    ('t' <$ matches ("span" @: ["title" @= titleAttrValue]))
      <|> pure 'f'
