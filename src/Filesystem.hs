-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0-or-later license found in the LICENSE.md file in the root directory
-- of this source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module Filesystem
    ( FilePathWithMime
    , buildLibrary
    ) where

import           Codec.MIME.Parse            ( parseMIMEType )
import           Codec.MIME.Type             ( MIMEType (Audio),
                                               Type (mimeType) )
import           Data.Maybe                  ( catMaybes )
import           Data.Text                   ( Text, pack, unpack )
import           Magic                       ( Magic, MagicFlag (MagicMime),
                                               magicFile, magicLoadDefault,
                                               magicOpen )
import           System.FilePath.Find        ( always, find )
import           System.FilePath.GlobPattern ( GlobPattern )

type FilePathWithMime = (FilePath, MIMEType)

search :: GlobPattern -> FilePath -> IO [FilePath]
search _ = find always (return True)

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
