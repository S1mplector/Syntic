module Syntic.Domain.Geometry
  ( CanvasSize
  , CanvasSizeError (..)
  , canvasWidth
  , canvasHeight
  , mkCanvasSize
  , Point (..)
  , Vector (..)
  , Rectangle (..)
  , Ellipse (..)
  , translatePoint
  , translateRectangle
  , translateEllipse
  ) where

import Numeric.Natural (Natural)

data CanvasSize = CanvasSize
  { internalCanvasWidth :: !Natural
  , internalCanvasHeight :: !Natural
  }
  deriving stock (Eq, Show)

data CanvasSizeError
  = CanvasWidthMustBePositive
  | CanvasHeightMustBePositive
  deriving stock (Eq, Show)

canvasWidth :: CanvasSize -> Natural
canvasWidth = internalCanvasWidth

canvasHeight :: CanvasSize -> Natural
canvasHeight = internalCanvasHeight

mkCanvasSize :: Natural -> Natural -> Either CanvasSizeError CanvasSize
mkCanvasSize width height
  | width == 0 = Left CanvasWidthMustBePositive
  | height == 0 = Left CanvasHeightMustBePositive
  | otherwise = Right (CanvasSize width height)

data Point = Point
  { pointX :: !Int
  , pointY :: !Int
  }
  deriving stock (Eq, Show)

data Vector = Vector
  { deltaX :: !Int
  , deltaY :: !Int
  }
  deriving stock (Eq, Show)

data Rectangle = Rectangle
  { rectangleOrigin :: !Point
  , rectangleWidth :: !Natural
  , rectangleHeight :: !Natural
  }
  deriving stock (Eq, Show)

data Ellipse = Ellipse
  { ellipseCenter :: !Point
  , ellipseRadiusX :: !Natural
  , ellipseRadiusY :: !Natural
  }
  deriving stock (Eq, Show)

translatePoint :: Vector -> Point -> Point
translatePoint (Vector deltaX deltaY) (Point pointX pointY) =
  Point
    { pointX = pointX + deltaX
    , pointY = pointY + deltaY
    }

translateRectangle :: Vector -> Rectangle -> Rectangle
translateRectangle offset rectangle =
  rectangle
    { rectangleOrigin = translatePoint offset (rectangleOrigin rectangle)
    }

translateEllipse :: Vector -> Ellipse -> Ellipse
translateEllipse offset ellipse =
  ellipse
    { ellipseCenter = translatePoint offset (ellipseCenter ellipse)
    }

