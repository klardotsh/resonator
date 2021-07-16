-- This source code is part of the resonator project, licensed under the CC0-1.0
-- license found in the COPYING file in the root directory of this source tree,
-- or at https://creativecommons.org/publicdomain/zero/1.0/
module CodecUtils
    ( Codec
    , LosslessCodec(..)
    , LossyCodec(..)
    ) where

type Codec = Either LosslessCodec LossyCodec

-- These values are pulled from `streams[].codec_name` in ffprobe's output, and
-- should be pascal-cased (for Text.Casing.toPascal-friendliness)
data LosslessCodec
    = Ape
    | Flac
    | PcmS8le -- WAV - does this even exist?
    | PcmS8be -- WAV - does this even exist?
    | PcmS16le -- WAV
    | PcmS16be -- WAV - does this even exist?
    | PcmS24le -- WAV
    | PcmS24be -- WAV - does this even exist?
    | PcmS32le -- WAV
    | PcmS32be -- WAV - does this even exist?
    | PcmS48le -- WAV - does this even exist?
    | PcmS48be -- WAV - does this even exist?
    deriving (Bounded, Enum, Eq, Read, Show)

-- These values are pulled from `streams[].codec_name` in ffprobe's output, and
-- should be pascal-cased (for Text.Casing.toPascal-friendliness)
data LossyCodec
    = Aac
    | Mp3
    | Speex
    | Vorbis
    deriving (Bounded, Enum, Eq, Read, Show)
