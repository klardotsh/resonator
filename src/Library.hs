-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0-or-later license found in the LICENSE.md file in the root directory
-- of this source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module Library
    ( LibraryTrack(..)
    , LibraryTrackError
    , LibraryTrackContainer(..)
    , LibraryTrackErrorOrContainer
    , fileForLibrary
    , groupableTag
    ) where

import           BitrateUtils            ( Kbps (..) )
import           CodecUtils              ( Codec, LosslessCodec (..),
                                           LossyCodec (..) )
import           Config                  ( ConfigurationLibraryGrouping (..) )
import           FFprobe                 ( FFprobeResult (..),
                                           FFprobeResultFormat (..),
                                           FFprobeResultStream (..),
                                           FFprobeResultTags (..), probeFile )
import           TimeUtils               ( Milliseconds (..) )

import           Control.Applicative     ( (<|>) )
import           Data.Maybe              ( fromMaybe )
import qualified Data.Set                as Set
import           Data.String.Conversions ( cs )
import           Data.Text               ( Text )
import qualified Data.Text               as T
import           Data.Text.Format        ( Only (..), format )
import           Data.Time.Clock         ( UTCTime )
import           System.Directory        ( getModificationTime )
import           Text.Casing             ( pascal )
import           Text.Read               ( readMaybe )

type LibraryTrackError = String

type LibraryTrackErrorOrContainer
     = Either LibraryTrackError LibraryTrackContainer

data LibraryGroupingType
    = AlbumArtist Text
    -- ^ while this seems to be more or less the same as Artist, at the glass it
    -- is sometimes desired to separate albums with multiple contributors into a
    -- "Various Artists" section, so we'll separate out *all* tracks with a
    -- populated AlbumArtist and let downstream consumers deal with it
    | Artist Text
    | Filename FilePath
    -- ^ represents a "No Name" grouping where files are only identifiable by
    -- their filename
    deriving (Show)

data LibraryTrackContainer
    = LibraryTrackContainer
          { filePath     :: FilePath
          , mtime        :: UTCTime
          , track        :: LibraryTrack
          , groupingType :: LibraryGroupingType
          }
    deriving (Show)

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

maybeEitherFromMaybes :: Maybe a -> Maybe b -> Maybe (Either a b)
maybeEitherFromMaybes l r = (l >>= (Just . Left)) <|> (r >>= (Just . Right))

uniqueCodecOrError :: [Text] -> Either LibraryTrackError Codec
uniqueCodecOrError codecs
    | null codecs =
        Left
            "could not identify track's codec (no streams - is this an audio file?)"
    | length codecs > 1 = uniqueCodecOrError [head codecs]
    | otherwise = do
        let codec = pascal . cs . head $ codecs
        maybe (Left . cs . format "unknown codec {}" $ Only codec) Right $
            maybeEitherFromMaybes
                (maybeLosslessCodec codec)
                (maybeLossyCodec codec)

durationOrError ::
       [FFprobeResultStream] -> Either LibraryTrackError Milliseconds
durationOrError streams
    | null streams =
        Left
            "could not identify track's duration (no streams - is this an audio file?)"
    | length streams > 1 = durationOrError [head streams]
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

trackFromFFprobe ::
       FilePath
    -> UTCTime
    -> Either String FFprobeResult
    -> LibraryTrackErrorOrContainer
trackFromFFprobe fp mtime possibleRes = do
    lt <- possibleRes >>= probeResultToLibraryTrack
    Right $ LibraryTrackContainer fp mtime lt $ calculateGroupingType fp lt

fileForLibrary :: FilePath -> IO LibraryTrackErrorOrContainer
fileForLibrary fp = do
    mtime <- getModificationTime fp
    probed <- probeFile fp
    return $ trackFromFFprobe fp mtime probed

calculateGroupingType :: FilePath -> LibraryTrack -> LibraryGroupingType
calculateGroupingType fp lt =
    fromMaybe (Filename fp) $ maybeArtistOrAlbumArtist lt

maybeArtistOrAlbumArtist :: LibraryTrack -> Maybe LibraryGroupingType
maybeArtistOrAlbumArtist lt =
    (ltAlbumArtist lt >>= (Just . AlbumArtist)) <|>
    (ltArtist lt >>= (Just . Artist))

groupableTag :: ConfigurationLibraryGrouping -> Text -> Text
groupableTag conf
    | not (groupingEnabled conf) = id
    | otherwise =
        transformSpaces (ignoreSpaces conf) .
        transformArticles (ignoreArticles conf) .
        transformSpecialCharacters (ignoreSpecialCharacters conf) .
        transformCase (ignoreCase conf)

conditionalTransform :: Bool -> (a -> a) -> a -> a
conditionalTransform False _ x = x
conditionalTransform True fn x = fn x

transformArticles :: Bool -> Text -> Text
transformArticles enabled =
    conditionalTransform enabled $ T.unwords . filter notArticle . T.words

transformCase :: Bool -> Text -> Text
transformCase enabled = conditionalTransform enabled T.toCaseFold

transformSpaces :: Bool -> Text -> Text
transformSpaces enabled = conditionalTransform enabled $ T.concat . T.words

transformSpecialCharacters :: Bool -> Text -> Text
transformSpecialCharacters enabled =
    conditionalTransform enabled $ T.filter notSpecialCharacter

-- entirely non-exhaustive and only handles English for now, because klardotsh
-- is only (arguably) fluent in English and doesn't trust his fuzzy
-- understanding of any other language enough to write grouping rules for it. as
-- always, patches welcome.
articles :: Set.Set Text
articles = Set.fromList ["a", "an", "the"]

-- drop article words: https://en.wikipedia.org/wiki/Article_(grammar)
-- regardless of `ignoreCase`'s impact on grouping the rest of the string, this
-- method drops articles case-insensitively
notArticle :: Text -> Bool
notArticle word = Set.notMember (T.toCaseFold word) articles

-- by no means exhaustive, or probably even quite correct, but for Latin script
-- languages, this should be a good start. as always, patches welcome.
--
-- sorted with vim's `:'<,'>sort`
specialCharacters :: Set.Set Char
specialCharacters =
    Set.fromList
        [ '!'
        , '"'
        , '$'
        , '%'
        , '&'
        , '('
        , ')'
        , '*'
        , '+'
        , ','
        , '-'
        , '.'
        , '/'
        , ':'
        , ';'
        , '<'
        , '>'
        , '@'
        , '['
        , '\''
        , '\\'
        , ']'
        , '^'
        , '_'
        , '{'
        , '|'
        , '}'
        ]

notSpecialCharacter :: Char -> Bool
notSpecialCharacter c = Set.notMember c specialCharacters
