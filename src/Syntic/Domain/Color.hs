module Syntic.Domain.Color
  ( Color (..)
  , opaque
  , transparent
  ) where

import Data.Word (Word8)

data Color = Color
  { red :: !Word8
  , green :: !Word8
  , blue :: !Word8
  , alpha :: !Word8
  }
  deriving stock (Eq, Show)

opaque :: Word8 -> Word8 -> Word8 -> Color
opaque red green blue = Color red green blue 255

transparent :: Color
transparent = Color 0 0 0 0

