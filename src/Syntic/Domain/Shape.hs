module Syntic.Domain.Shape
  ( Shape (..)
  , ShapeGeometry (..)
  , Style (..)
  , Stroke (..)
  , moveShape
  ) where

import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Syntic.Domain.BrushStroke (BrushStroke, translateBrushStroke)
import Syntic.Domain.Color (Color)
import Syntic.Domain.Geometry
  ( Ellipse
  , Point
  , Rectangle
  , Vector
  , translateEllipse
  , translatePoint
  , translateRectangle
  )
import Syntic.Domain.Identifier (ShapeId)

data Shape = Shape
  { shapeId :: !ShapeId
  , shapeGeometry :: !ShapeGeometry
  , shapeStyle :: !Style
  }
  deriving stock (Eq, Show)

data ShapeGeometry
  = RectangleGeometry !Rectangle
  | EllipseGeometry !Ellipse
  | PolylineGeometry !(NonEmpty Point)
  | BrushStrokeGeometry !BrushStroke
  deriving stock (Eq, Show)

data Style = Style
  { fillColor :: !(Maybe Color)
  , stroke :: !(Maybe Stroke)
  }
  deriving stock (Eq, Show)

data Stroke = Stroke
  { strokeColor :: !Color
  , strokeWidth :: !Natural
  }
  deriving stock (Eq, Show)

moveShape :: Vector -> Shape -> Shape
moveShape offset shape =
  shape
    { shapeGeometry = moveGeometry offset (shapeGeometry shape)
    }

moveGeometry :: Vector -> ShapeGeometry -> ShapeGeometry
moveGeometry offset geometry =
  case geometry of
    RectangleGeometry rectangle ->
      RectangleGeometry (translateRectangle offset rectangle)
    EllipseGeometry ellipse ->
      EllipseGeometry (translateEllipse offset ellipse)
    PolylineGeometry points ->
      PolylineGeometry (fmap (translatePoint offset) points)
    BrushStrokeGeometry brushStroke ->
      BrushStrokeGeometry (translateBrushStroke offset brushStroke)
