-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ProjectMeta
    ( CopyrightInfo(..)
    , linkToProject
    , projectName
    , copyrightInfo
    ) where

import qualified Data.Aeson       as Json
import           Data.Text        ( Text )
import           Data.Text.Format ( Format, Only (..), format )
import           Data.Text.Lazy   ( toStrict )
import           GHC.Generics     ( Generic )

data CopyrightInfo
    = CopyrightInfo
          { copyrightString :: Text
          , license         :: Text
          , projectUrl      :: Text
          , sourceUrl       :: Text
          }
    deriving (Generic, Show, Json.ToJSON)

linkToProject :: Format
linkToProject = "https://git.klar.sh/klardotsh/{}"

projectName :: String
projectName = "resonator"

copyrightInfo :: CopyrightInfo
copyrightInfo =
    CopyrightInfo
        { copyrightString = "(c) 2020 Josh Klar a.k.a. klardotsh"
        , license = "CC0-1.0: https://creativecommons.org/publicdomain/zero/1.0/"
        , projectUrl = toStrict $ format linkToProject $ Only projectName
        , sourceUrl = toStrict $ format linkToProject $ Only projectName
        }
