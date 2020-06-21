-- This source code is part of the resonator project, licensed under the
-- AGPL-3.0-or-later license found in the LICENSE.md file in the root directory
-- of this source tree, or at https://www.gnu.org/licenses/agpl-3.0.html
module BitrateUtils
    ( Kbps(..)
    , BitrateExceededBehavior(..)
    ) where

-- all bitrates in resonator are expressed in kbps (little b - kilobits)
newtype Kbps =
    Kbps
        { getKbps :: Int
        }
    deriving (Eq, Num, Ord, Read, Show)

data BitrateExceededBehavior
    = LowerBitrateWithNotice
    | LogWarningAndMoveOn
    | Fatal
    deriving (Bounded, Enum, Eq, Read, Show)
