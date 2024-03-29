-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
module Config
    ( findDefaultConfigFile
    , readConfigFromFile
    , Configuration(..)
    , ConfigurationAuth(..)
    , ConfigurationClients(..)
    , ConfigurationCollection(..)
    , ConfigurationLibraryGrouping(..)
    ) where

import           BitrateUtils           ( BitrateExceededBehavior (..),
                                          Kbps (..) )
import           ProjectMeta            ( projectName )
import           TextUtils              ( showAsKebab )
import           TimeUtils              ( Milliseconds (..), weeks )

import           Control.Monad.IO.Class ( MonadIO )
import           Data.Coerce            ( Coercible )
import           Data.Text              ( Text, unpack )
import           Data.Text.Format       ( Only (..), format )
import           Data.Text.Lazy         ( toStrict )
import           System.Directory       ( XdgDirectory (XdgConfig),
                                          getXdgDirectory )
import           System.FilePath        ( (<.>), (</>) )
import           Text.Casing            ( pascal )
import           Text.Read              ( readMaybe )
import           Toml                   ( (.=) )
import qualified Toml                   as T
import           Toml.Codec.Types       ( Codec (..), TomlCodec, (<!>) )

readConfigFromFile ::
       (MonadIO m) => FilePath -> m (Either [T.TomlDecodeError] Configuration)
readConfigFromFile = T.decodeFileEither codec

findDefaultConfigFile :: IO FilePath
findDefaultConfigFile =
    getXdgDirectory XdgConfig $ projectName </> "config" <.> "toml"

withDefault :: a -> TomlCodec a -> TomlCodec a
withDefault def Codec {..} =
    Codec {codecRead = codecRead <!> \_ -> pure def, codecWrite = codecWrite}

withDefaultBool :: Bool -> T.Key -> T.Codec Bool Bool
withDefaultBool val key = withDefault val (T.bool key)

withDefaultIntLike :: Coercible o Int => (t -> o) -> t -> T.Key -> Codec o o
withDefaultIntLike cls val key = withDefault (cls val) (T.diwrap (T.int key))

unknownBEB :: Text -> Text
unknownBEB str =
    toStrict . format "unknown BitrateExceededBehavior: {}" $ Only str

-- This is because I wanted to keep the config file kebab-case and not have enum
-- members magically flip to PascalCase - otherwise, there's already a tomland
-- codec for Bounded derivatives, and `max-bitrate-exceeded-request-behavior =
-- "LowerBitrateWithNotice"` would Just Work.
bebFromTomlString :: Text -> Either Text BitrateExceededBehavior
bebFromTomlString str =
    maybe
        (Left $ unknownBEB str)
        Right
        (readMaybe $ (pascal . unpack) str :: Maybe BitrateExceededBehavior)

data Configuration
    = Configuration
          { auth        :: ConfigurationAuth
          , clients     :: ConfigurationClients
          , collections :: [ConfigurationCollection]
          , library     :: ConfigurationLibrary
          }
    deriving (Show)

data ConfigurationAuth
    = ConfigurationAuth
          { secret                  :: Maybe Text
          , tokenExpiryMilliseconds :: Milliseconds
          }
    deriving (Show)

data ConfigurationClients
    = ConfigurationClients
          { allowSourceDownload :: Bool
          , allowLossless :: Bool
          , allowLossy :: Bool
          , allowTranscodingLossySources :: Bool
          , allowTranscodingLossySourcesMinBitrateKbps :: Kbps
          , maxBitrateKbps :: Kbps
          , maxBitrateExceededRequestBehavior :: BitrateExceededBehavior
          , maxBitrateExceededInadvertentBehavior :: BitrateExceededBehavior
          }
    deriving (Show)

data ConfigurationCollection
    = ConfigurationCollection
          { name                   :: Text
          , directory              :: Text
          , followSymlinksExternal :: Bool
          , followSymlinksInternal :: Bool
          , watch                  :: Bool
          }
    deriving (Show)

data ConfigurationLibrary
    = ConfigurationLibrary
          { enableCueFiles :: Bool
          , grouping       :: ConfigurationLibraryGrouping
          }
    deriving (Show)

data ConfigurationLibraryGrouping
    = ConfigurationLibraryGrouping
          { groupingEnabled         :: Bool
          , ignoreArticles          :: Bool
          , ignoreCase              :: Bool
          , ignoreSpaces            :: Bool
          , ignoreSpecialCharacters :: Bool
          , groupByAlbumArtist      :: Bool
          }
    deriving (Show)

codec :: TomlCodec Configuration
codec =
    Configuration <$> T.table authCodec "auth" .= auth <*>
    T.table clientsCodec "clients" .= clients <*>
    T.list collectionCodec "collections" .= collections <*>
    T.table libraryCodec "library" .= library

authCodec :: TomlCodec ConfigurationAuth
authCodec =
    ConfigurationAuth <$> T.dioptional (T.text "secret") .= secret <*>
    withDefaultIntLike
        Milliseconds
        (getMilliseconds $ weeks 1)
        "token-expiry-milliseconds" .=
    tokenExpiryMilliseconds

clientsCodec :: TomlCodec ConfigurationClients
clientsCodec =
    ConfigurationClients <$>
    withDefaultBool False "allow-source-download" .= allowSourceDownload <*>
    withDefaultBool False "allow-lossless" .= allowLossless <*>
    withDefaultBool True "allow-lossy" .= allowLossy <*>
    withDefaultBool False "allow-transcoding-lossy-sources" .=
    allowTranscodingLossySources <*>
    withDefaultIntLike
        Kbps
        256
        "allow-transcoding-lossy-sources-min-bitrate-kbps" .=
    allowTranscodingLossySourcesMinBitrateKbps <*>
    withDefaultIntLike Kbps 320 "max-bitrate-kbps" .= maxBitrateKbps <*>
    withDefault
        LowerBitrateWithNotice
        (T.textBy
             showAsKebab
             bebFromTomlString
             "max-bitrate-exceeded-request-behavior") .=
    maxBitrateExceededRequestBehavior <*>
    withDefault
        LogWarningAndMoveOn
        (T.textBy
             showAsKebab
             bebFromTomlString
             "max-bitrate-exceeded-inadvertent-behavior") .=
    maxBitrateExceededInadvertentBehavior

collectionCodec :: TomlCodec ConfigurationCollection
collectionCodec =
    ConfigurationCollection <$> T.text "name" .= name <*>
    T.text "directory" .= directory <*>
    withDefaultBool False "follow-symlinks-external" .= followSymlinksExternal <*>
    withDefaultBool False "follow-symlinks-internal" .= followSymlinksInternal <*>
    withDefaultBool False "watch" .= watch

libraryCodec :: TomlCodec ConfigurationLibrary
libraryCodec =
    ConfigurationLibrary <$>
    withDefaultBool True "enable-cue-files" .= enableCueFiles <*>
    T.table libraryGroupingCodec "grouping" .= grouping

libraryGroupingCodec :: TomlCodec ConfigurationLibraryGrouping
libraryGroupingCodec =
    ConfigurationLibraryGrouping <$>
    withDefaultBool True "enabled" .= groupingEnabled <*>
    withDefaultBool True "ignore-articles" .= ignoreArticles <*>
    withDefaultBool True "ignore-case" .= ignoreCase <*>
    withDefaultBool True "ignore-spaces" .= ignoreSpaces <*>
    withDefaultBool True "ignore-special-characters" .= ignoreSpecialCharacters <*>
    withDefaultBool True "group-by-album-artist" .= groupByAlbumArtist
