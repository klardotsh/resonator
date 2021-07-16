-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
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
