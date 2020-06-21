-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0-or-later license found in the LICENSE.md file in the root directory
-- of this source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module Filesystem
    ( FilePathWithMime
    , buildLibrary
    ) where

import           BitrateUtils                ( Kbps (..) )
import           CodecUtils                  ( LosslessCodec (..),
                                               LossyCodec (..) )
import           FFprobe
import           TimeUtils                   ( Milliseconds (..) )

import           Codec.MIME.Parse            ( parseMIMEType )
import           Codec.MIME.Type             ( MIMEType (Audio),
                                               Type (mimeType) )
import           Control.Applicative         ( (<|>) )
import           Control.Monad               ( mapM )
import           Data.List                   ( nub )
import           Data.Maybe                  ( catMaybes, fromMaybe )
import           Data.Text                   ( Text, pack, unpack )
import           Magic                       ( Magic, MagicFlag (MagicMime),
                                               magicFile, magicLoadDefault,
                                               magicOpen )
import           System.FilePath.Find        ( FilterPredicate, always,
                                               filePath, find )
import           System.FilePath.GlobPattern ( GlobPattern )
import           Text.Pretty.Simple          ( pPrint )
import           Text.Read                   ( readMaybe )

type FilePathWithMime = (FilePath, MIMEType)

search :: GlobPattern -> FilePath -> IO [FilePath]
search pat = find always (return True)

isAudioMime :: (a, MIMEType) -> Bool
isAudioMime (_, Audio _) = True
isAudioMime _            = False

bindFileMime :: Magic -> FilePath -> IO (Maybe FilePathWithMime)
bindFileMime magic file = do
    mime <- magicFile magic file
    return $ parseMIMEType (pack mime) >>= \x -> Just (file, mimeType x)

audioFilesInTree :: Magic -> Text -> IO [FilePath]
audioFilesInTree magic path = do
    files <- search "*" $ unpack path
    mapped <- catMaybes <$> mapM (bindFileMime magic) files
    return $ map fst . filter isAudioMime $ mapped

audioFileSearch :: Text -> IO [FilePath]
audioFileSearch path = do
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    audioFilesInTree magic path

buildLibrary :: [Text] -> IO [FilePath]
buildLibrary paths = concat <$> mapM audioFileSearch paths
