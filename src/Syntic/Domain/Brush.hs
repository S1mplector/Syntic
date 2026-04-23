module Syntic.Domain.Brush
  ( Brush (..)
  , BrushTip (..)
  , defaultWatercolorBrush
  , washBrush
  , detailBrush
  ) where

data BrushTip
  = RoundTip
  | FlatTip
  | MopTip
  deriving stock (Eq, Show)

data Brush = Brush
  { brushTip            :: !BrushTip
  , brushSize           :: !Double    -- diameter in canvas units (> 0)
  , brushOpacity        :: !Double    -- 0.0 – 1.0
  , brushFlow           :: !Double    -- pigment flow rate  0.0 – 1.0
  , brushWetness        :: !Double    -- water content      0.0 – 1.0
  , brushPigmentDensity :: !Double    -- pigment load       0.0 – 1.0
  , brushGranulation    :: !Double    -- paper-texture gain  0.0 – 1.0
  , brushSoftness       :: !Double    -- edge feathering     0.0 – 1.0
  }
  deriving stock (Eq, Show)

defaultWatercolorBrush :: Brush
defaultWatercolorBrush =
  Brush
    { brushTip            = RoundTip
    , brushSize           = 12.0
    , brushOpacity        = 0.7
    , brushFlow           = 0.6
    , brushWetness        = 0.5
    , brushPigmentDensity = 0.8
    , brushGranulation    = 0.3
    , brushSoftness       = 0.6
    }

washBrush :: Brush
washBrush =
  Brush
    { brushTip            = MopTip
    , brushSize           = 48.0
    , brushOpacity        = 0.35
    , brushFlow           = 0.4
    , brushWetness        = 0.85
    , brushPigmentDensity = 0.3
    , brushGranulation    = 0.15
    , brushSoftness       = 0.9
    }

detailBrush :: Brush
detailBrush =
  Brush
    { brushTip            = RoundTip
    , brushSize           = 3.0
    , brushOpacity        = 0.9
    , brushFlow           = 0.8
    , brushWetness        = 0.2
    , brushPigmentDensity = 0.95
    , brushGranulation    = 0.5
    , brushSoftness       = 0.2
    }
