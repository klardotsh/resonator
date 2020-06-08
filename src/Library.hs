-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0 license found in the LICENSE.md file in the root directory of this
-- source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module Library
    ( LibraryTrack(..)
    , LibraryTrackError
    , fileForLibrary
    ) where

import           BitrateUtils            ( Kbps (..) )
import           CodecUtils              ( Codec, LosslessCodec (..),
                                           LossyCodec (..) )
import           FFprobe                 ( FFprobeResult (..),
                                           FFprobeResultFormat (..),
                                           FFprobeResultStream (..),
                                           FFprobeResultTags (..), probeFile )
import           TimeUtils               ( Milliseconds (..) )

import           Data.ByteString.Lazy    ( ByteString )
import           Data.List               ( nub )
import           Data.Maybe              ( fromMaybe )
import           Data.String.Conversions ( cs )
import           Data.Text               ( Text )
import           Data.Text.Format        ( Format, Only (..), format )
import           Data.Text.Lazy          ( toStrict )
import           System.Process.Typed    ( readProcess, shell )
import           Text.Casing             ( pascal )
import           Text.Read               ( readMaybe )

data LibraryTrack
    = LibraryTrack
          { ltTitle       :: Maybe Text
          , ltArtist      :: Maybe Text
          , ltAlbumArtist :: Maybe Text
          , ltAlbum       :: Maybe Text
          , ltGenre       :: Maybe Text
          , ltYear        :: Maybe Int
          , ltTrack       :: Maybe Int
          , ltTotalTracks :: Maybe Int
          -- ^ this is what the track claims, but we generally don't care
          , ltDisc        :: Maybe Int
          , ltTotalDiscs  :: Maybe Int
          -- ^ this is what the track claims, but we generally don't care
          , ltDuration    :: Milliseconds
          , ltCodec       :: Codec
          , ltBitrate     :: Kbps
          }
    deriving (Show)

type LibraryTrackError = String

tagsOrError ::
       Maybe FFprobeResultTags -> Either LibraryTrackError FFprobeResultTags
tagsOrError (Just fmt) = Right fmt
tagsOrError _          = Left "ffprobe could not parse tags from track"

maybeTextToMaybeInt :: Maybe Text -> Maybe Int
maybeTextToMaybeInt x = readMaybe $ cs $ fromMaybe "" x :: Maybe Int

maybeLosslessCodec :: String -> Maybe LosslessCodec
maybeLosslessCodec c = readMaybe c :: Maybe LosslessCodec

maybeLossyCodec :: String -> Maybe LossyCodec
maybeLossyCodec c = readMaybe c :: Maybe LossyCodec

-- someone who is actually good at Haskell: is there a cleaner way to do this
-- pattern?
maybeEitherFromMaybes :: Maybe a -> Maybe b -> Maybe (Either a b)
maybeEitherFromMaybes (Just it) Nothing = Just $ Left it
maybeEitherFromMaybes Nothing (Just it) = Just $ Right it
maybeEitherFromMaybes _ _               = Nothing

unknownCodecErrorStr :: Format
unknownCodecErrorStr = "unknown codec {}"

uniqueCodecOrError :: [Text] -> Either LibraryTrackError Codec
uniqueCodecOrError codecs
    | (null . nub) codecs =
        Left
            "could not identify track's codec (no streams - is this an audio file?)"
    | (length . nub) codecs > 1 = uniqueCodecOrError [head codecs]
    | otherwise = do
        let codec = pascal . cs . head $ codecs
        maybe (Left . cs . format unknownCodecErrorStr $ Only codec) Right $
            maybeEitherFromMaybes
                (maybeLosslessCodec codec)
                (maybeLossyCodec codec)

durationOrError ::
       [FFprobeResultStream] -> Either LibraryTrackError Milliseconds
durationOrError streams
    | (null . nub) streams =
        Left
            "could not identify track's duration (no streams - is this an audio file?)"
    | (length . nub) streams > 1 = durationOrError [head streams]
    | otherwise = do
        let stream = head streams
        Right $
            Milliseconds $
            floor $ (read . cs . ffprsDuration $ stream :: Float) * 1000

probeResultToLibraryTrack ::
       FFprobeResult -> Either LibraryTrackError LibraryTrack
probeResultToLibraryTrack probed = do
    tags <- tagsOrError $ ffprfTags $ ffprFormat probed
    ltTitle <- Right $ ffprtTitle tags
    ltArtist <- Right $ ffprtArtist tags
    ltAlbumArtist <- Right $ ffprtAlbumArtist tags
    ltAlbum <- Right $ ffprtAlbum tags
    ltGenre <- Right $ ffprtGenre tags
    ltTrack <- Right . maybeTextToMaybeInt . ffprtTrack $ tags
    ltTotalTracks <- Right . maybeTextToMaybeInt . ffprtTotalTracks $ tags
    ltDisc <- Right . maybeTextToMaybeInt . ffprtDisc $ tags
    ltTotalDiscs <- Right . maybeTextToMaybeInt . ffprtTotalDiscs $ tags
    ltCodec <- uniqueCodecOrError . map ffprsCodecName . ffprStreams $ probed
    let rawBitrate = cs . ffprfBitrate . ffprFormat $ probed
    ltBitrate <- Right . Kbps . floor $ (read rawBitrate :: Float) / 1000
    -- there has to be a better way than these couple lines......
    --
    -- Also: taking 4 is nowhere near safe, but it's the closest hope we really
    -- have to extracting just the year from a free-form text field. In the wild
    -- this field is *usually* `YYYY` or `YYYY-MM-DD` format...
    let rawYear = take 4 . cs <$> ffprtDate tags
    ltYear <- Right (readMaybe (fromMaybe "" rawYear) :: Maybe Int)
    ltDuration <- durationOrError $ ffprStreams probed
    return LibraryTrack {..}

fileForLibrary :: FilePath -> IO (Either String LibraryTrack)
fileForLibrary p = do
    probed <- probeFile p
    case probed of
        Left e    -> return $ Left e
        Right res -> return $ probeResultToLibraryTrack res
