-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0-or-later license found in the LICENSE.md file in the root directory
-- of this source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module FFprobe
    ( FFprobeResult(..)
    , FFprobeResultFormat(..)
    , FFprobeResultStream(..)
    , FFprobeResultTags(..)
    , probeFile
    ) where

import           Control.Applicative     ( (<|>) )
import           Control.Monad           ( mzero )
import           Data.Aeson              ( FromJSON, eitherDecode, parseJSON,
                                           (.:), (.:?) )
import qualified Data.Aeson.Types        as AT
import           Data.ByteString.Lazy    ( ByteString )
import           Data.String.Conversions ( cs )
import           Data.Text               ( Text )
import           Data.Text.Format        ( Format, Only (..), format )
import           System.Directory        ( findExecutable )
import           System.Exit             ( ExitCode (ExitFailure) )
import           System.Process.Typed    ( ProcessConfig, proc, readProcess,
                                           shell )
import           Text.Pretty.Simple      ( pPrint )

data FFprobeResult
    = FFprobeResult
          { ffprFormat  :: FFprobeResultFormat
          , ffprStreams :: [FFprobeResultStream]
          }
    deriving (Eq, Show)

instance FromJSON FFprobeResult where
    parseJSON (AT.Object v) = do
        ffprFormat <- v .: "format"
        ffprStreams <- v .: "streams"
        return FFprobeResult {..}
    parseJSON _ = mzero

data FFprobeResultFormat
    = FFprobeResultFormat
          { ffprfSize    :: Text
          , ffprfTags    :: Maybe FFprobeResultTags
          , ffprfBitrate :: Text
          }
    deriving (Eq, Show)

instance FromJSON FFprobeResultFormat where
    parseJSON (AT.Object v) = do
        ffprfSize <- v .: "size"
        ffprfTags <- v .:? "tags"
        ffprfBitrate <- v .: "bit_rate"
        return FFprobeResultFormat {..}
    parseJSON _ = mzero

-- All tags are optional, because some collections are incompletely,
-- inconsistently, or not at all tagged. Even people who really care about
-- having a sanely-tagged library will probably have a few files missing at
-- least one of these fields.
data FFprobeResultTags
    = FFprobeResultTags
          { ffprtDate        :: Maybe Text
          , ffprtAlbum       :: Maybe Text
          , ffprtAlbumArtist :: Maybe Text
          , ffprtArtist      :: Maybe Text
          , ffprtTitle       :: Maybe Text
          , ffprtGenre       :: Maybe Text
          , ffprtTrack       :: Maybe Text
          , ffprtTotalTracks :: Maybe Text
          , ffprtDisc        :: Maybe Text
          , ffprtTotalDiscs  :: Maybe Text
          }
    deriving (Eq, Show)

-- ffprobe is fairly unopinionated with its output, which is really unfortunate:
-- it passes through ID3/FLAC/etc. tags verbatim, which leads to some of the
-- wild mixes of snake_case and WHATEVERTHISIS. Regardless, the last condition
-- in the Applicative chain should always use .:?, and should be the ONLY call
-- to use .:?, so that Nothing values propagate up, but only as an absolute last
-- resort (i.e. none of the tags we know to represent that field are populated)
--
-- Why, you ask, not just use taglib, which (1) IS opinionated, and (2) has a
-- really nice Haskell binding? Mostly because it doesn't support all the tags
-- we need - from memory, I believe AlbumArtist and Disc/TotalDiscs were missing
-- (at least from htaglib). If I'm wrong, now or in the future, I'd totally love
-- to throw out this entire module, but for now, we have what we have.
instance FromJSON FFprobeResultTags where
    parseJSON (AT.Object v) = do
        ffprtDate <- v .: "DATE" <|> v .:? "date"
        ffprtAlbum <- v .: "ALBUM" <|> v .:? "album"
        ffprtAlbumArtist <- v .: "ALBUMARTIST" <|> v .:? "album_artist"
        ffprtArtist <- v .: "ARTIST" <|> v .:? "artist"
        ffprtTitle <- v .: "TITLE" <|> v .:? "title"
        ffprtGenre <- v .: "GENRE" <|> v .:? "genre"
        ffprtTrack <- v .: "TRACK" <|> v .:? "track"
        ffprtTotalTracks <- v .: "TOTALTRACKS" <|> v .:? "total_tracks"
        ffprtDisc <- v .: "DISC" <|> v .:? "disc"
        ffprtTotalDiscs <- v .: "TOTALDISCS" <|> v .:? "total_discs"
        return FFprobeResultTags {..}
    parseJSON _ = mzero

data FFprobeResultStream
    = FFprobeResultStream
          { ffprsCodecName :: Text
          , ffprsCodecType :: Text
          , ffprsDuration  :: Text
          }
    deriving (Eq, Show)

instance FromJSON FFprobeResultStream where
    parseJSON (AT.Object v) = do
        ffprsCodecName <- v .: "codec_name"
        ffprsCodecType <- v .: "codec_type"
        ffprsDuration <- v .: "duration"
        return FFprobeResultStream {..}
    parseJSON _ = mzero

parseFFprobeResponse :: ByteString -> Either String FFprobeResult
parseFFprobeResponse response =
    eitherDecode response :: Either String FFprobeResult

probeCommand :: Format
probeCommand =
    "ffprobe -v quiet -print_format json -show_format -show_streams '{}'"

generateProc :: String -> String -> ProcessConfig () () ()
generateProc ffprobe path =
    proc
        ffprobe
        [ "-v"
        , "quiet"
        , "-print_format"
        , "json"
        , "-show_format"
        , "-show_streams"
        , path
        ]

-- for now, jam finding ffprobe here. TODO FIXME this needs to live in
-- config.toml with a fallback to `which`
probeFile :: FilePath -> IO (Either String FFprobeResult)
probeFile path = do
    ffprobe <- findExecutable "ffprobe"
    case ffprobe of
        Just ffprobePath -> doProbeFile ffprobePath path
        Nothing          -> return $ Left "could not find ffprobe in $PATH"

doProbeFile :: String -> FilePath -> IO (Either String FFprobeResult)
doProbeFile ffprobe path = do
    (exitCode, out, err) <- readProcess $ generateProc ffprobe path
    case exitCode of
        ExitFailure _ -> (return . Left . cs) err
        _             -> (return . parseFFprobeResponse) out
