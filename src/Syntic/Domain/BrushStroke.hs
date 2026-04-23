module Syntic.Domain.BrushStroke
  ( BrushStroke (..)
  , StrokeSample (..)
  , translateBrushStroke
  ) where

import Data.List.NonEmpty (NonEmpty)
import Syntic.Domain.Brush (Brush)
import Syntic.Domain.Color (Color)
import Syntic.Domain.Geometry (Point, Vector, translatePoint)

data StrokeSample = StrokeSample
  { samplePosition :: !Point
  , samplePressure :: !Double   -- 0.0 – 1.0 (tablet / mouse)
  }
  deriving stock (Eq, Show)

data BrushStroke = BrushStroke
  { strokeBrush   :: !Brush
  , strokeColor   :: !Color
  , strokeSamples :: !(NonEmpty StrokeSample)
  }
  deriving stock (Eq, Show)

translateBrushStroke :: Vector -> BrushStroke -> BrushStroke
translateBrushStroke offset brushStroke =
  brushStroke
    { strokeSamples = fmap (translateSample offset) (strokeSamples brushStroke)
    }

translateSample :: Vector -> StrokeSample -> StrokeSample
translateSample offset sample =
  sample { samplePosition = translatePoint offset (samplePosition sample) }
