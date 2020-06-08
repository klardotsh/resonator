-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0 license found in the LICENSE.md file in the root directory of this
-- source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module TimeUtils
    ( Milliseconds(..)
    , seconds
    , minutes
    , hours
    , days
    , weeks
    ) where

-- all times in resonator are internally represented in milliseconds - display
-- in other formats is up to the client
newtype Milliseconds =
    Milliseconds
        { getMilliseconds :: Int
        }
    deriving (Eq, Num, Ord, Read, Show)

seconds :: Int -> Milliseconds
seconds s = Milliseconds $ s * 1000

minutes :: Int -> Milliseconds
minutes m = seconds $ m * 60

hours :: Int -> Milliseconds
hours h = minutes $ h * 60

days :: Int -> Milliseconds
days d = hours $ d * 24

weeks :: Int -> Milliseconds
weeks w = days $ w * 7
