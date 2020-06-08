-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0 license found in the LICENSE.md file in the root directory of this
-- source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module TextUtils
    ( showAsKebab
    , showAsKebabPair
    ) where

import           Data.Text   ( Text, pack )
import           Text.Casing ( kebab )

showAsKebab :: (Show a) => a -> Text
showAsKebab a = pack $ kebab $ show a

showAsKebabPair :: (Show a) => a -> (Text, a)
showAsKebabPair a = (showAsKebab a, a)
