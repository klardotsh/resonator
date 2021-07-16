-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
module ResonatorDaemon
    ( runApp
    ) where

import           Config             ( Configuration,
                                      Configuration (collections),
                                      ConfigurationCollection (directory),
                                      findDefaultConfigFile, readConfigFromFile )
import           Filesystem         ( buildLibrary )
import           Library            ( fileForLibrary )
import           ProjectMeta        ( copyrightInfo )

import           Data.Aeson         ( Value (..), object, (.=) )
import           Paths_resonator    ( version )
import           System.Environment ( lookupEnv )
import           Text.Pretty.Simple ( pPrint )
import qualified Toml               as T
import           UnliftIO.Async     ( pooledMapConcurrentlyN )
import qualified Web.Scotty         as S

configParseError :: [T.TomlDecodeError] -> IO ()
configParseError = pPrint

serve :: Configuration -> S.ScottyM ()
serve _ = do
    S.get "/" $
        S.json $ object ["licenseNotice" .= copyrightInfo, "version" .= version]
    S.get "/some-json" $
        S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

runAppFromConfig :: Configuration -> IO ()
runAppFromConfig config = do
    portVar <- lookupEnv "PORT"
    let port = maybe 8080 (\p -> read p :: Int) portVar
    pPrint config
    library <- buildLibrary $ map directory $ collections config
    responses <- pooledMapConcurrentlyN 8 fileForLibrary library
    -- dump failed files and their errors to debug log for now
    mapM_ pPrint $ zip library responses
    S.scotty port $ serve config

runApp :: IO ()
runApp = do
    configuration <- findDefaultConfigFile >>= readConfigFromFile
    either configParseError runAppFromConfig configuration
