-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
module TextUtils
    ( showAsKebab
    ) where

import           Data.Text   ( Text, pack )
import           Text.Casing ( kebab )

showAsKebab :: (Show a) => a -> Text
showAsKebab a = pack $ kebab $ show a
