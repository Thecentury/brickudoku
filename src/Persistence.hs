{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Persistence (saveToFile, loadFromFileIfExists) where

import Data.ByteString as B ( concat )
import Data.ByteString.Lazy as BL ( toChunks )
import Data.Text as T ( Text, pack, unpack )
import Data.Text.Encoding as T ( encodeUtf8, decodeUtf8 )
import GHC.Generics ( Generic )
import Data.Array ( Ix, bounds, elems, listArray, Array )
import Data.Aeson ( FromJSON(..), ToJSON(..), toEncoding, Value (..), genericToEncoding, defaultOptions, eitherDecodeStrict, encode )
import Data.Aeson.Encoding (int)
import Linear (V2)
import System.Random.Internal ( StdGen(..), unStdGen )
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import GHC.Stack (HasCallStack)

import Brickudoku
    ( Cell(..), FigureInSelection, VersionedState, GameState, Game )
import Undo (History)

data ArrayWrapper i a = ArrayWrapper
  { _bounds :: (i, i),
    _elems :: [a] }
    deriving (Generic, Show, FromJSON, ToJSON)

wrapArray :: Array i a -> ArrayWrapper i a
wrapArray a = ArrayWrapper (bounds a) (elems a)

unwrapArray :: Ix i => ArrayWrapper i a -> Array i a
unwrapArray (ArrayWrapper b e) = listArray b e

instance (Ix i, FromJSON i, FromJSON a) => FromJSON (Array i a) where
  parseJSON = fmap unwrapArray . parseJSON

instance (Ix i, ToJSON i, ToJSON a) => ToJSON (Array i a) where
  toEncoding a = toEncoding $ wrapArray a
  toJSON a = toJSON $ wrapArray a

instance ToJSON Cell where
  toEncoding Free = int 0
  toEncoding Filled = int 1
  
  toJSON Free = Number 0
  toJSON Filled = Number 1

instance FromJSON Cell where
  parseJSON (Number 0) = pure Free
  parseJSON (Number 1) = pure Filled
  parseJSON _ = fail "Cell must be 0 or 1"

instance ToJSON a => ToJSON (V2 a)
instance FromJSON a => FromJSON (V2 a)

instance ToJSON FigureInSelection
instance FromJSON FigureInSelection

instance ToJSON GameState
instance FromJSON GameState

instance ToJSON VersionedState
instance FromJSON VersionedState

instance ToJSON a => ToJSON (History a)
instance FromJSON a => FromJSON (History a)

instance ToJSON Game
instance FromJSON Game

data PersistedGame = PersistedGame 
  { _game :: Game, 
    _rng :: Text }
  deriving (Generic, Show)

stdGenFromString :: Text -> StdGen
stdGenFromString = StdGen . read . T.unpack

stdGenToString :: StdGen -> Text
stdGenToString = T.pack . show . unStdGen

persistedGameToGame :: PersistedGame -> (Game, StdGen)
persistedGameToGame (PersistedGame g gen) = (g, stdGenFromString gen)

gameToPersistedGame :: Game -> StdGen -> PersistedGame
gameToPersistedGame g gen = PersistedGame g $ stdGenToString gen

instance FromJSON PersistedGame
instance ToJSON PersistedGame where
  toEncoding = genericToEncoding defaultOptions

loadFromJSON :: Text -> Either String (Game, StdGen)
loadFromJSON = fmap persistedGameToGame . eitherDecodeStrict . T.encodeUtf8

saveToJSON :: Game -> StdGen -> Text
saveToJSON game gen = T.decodeUtf8 . B.concat . BL.toChunks . encode $ gameToPersistedGame game gen

savesDirectory :: HasCallStack => IO FilePath
savesDirectory = do
  home <- getHomeDirectory
  let dir = home </> ".config" </> "brickudoku"
  createDirectoryIfMissing True dir
  return dir

saveFile :: FilePath
saveFile = "save.json"

saveFilePath :: HasCallStack => IO FilePath
saveFilePath = (</> saveFile) <$> savesDirectory

saveToFile :: HasCallStack => Game -> StdGen -> IO ()
saveToFile game gen = do
  path <- saveFilePath
  writeFile path $ T.unpack $ saveToJSON game gen

saveFileExists :: HasCallStack => IO Bool
saveFileExists = do
  path <- saveFilePath
  doesFileExist path

loadFromFile :: IO (Either String (Game, StdGen))
loadFromFile = do
  path <- saveFilePath
  fmap loadFromJSON $ T.pack <$> readFile path

loadFromFileIfExists :: HasCallStack => IO (Maybe (Either String (Game, StdGen)))
loadFromFileIfExists = do
  exists <- saveFileExists
  if exists
    then Just <$> loadFromFile
    else return Nothing