module Syntic.Adapter.Gtk.Render
  ( renderDocument
  , renderDragPreview
  ) where

import Control.Monad (forM_, when)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8)
import qualified GI.Cairo.Render as Cairo
import Syntic.Adapter.Gtk.State
  ( AppState (asBrushPreset, asColor, asDrag)
  , DragState (BrushStroking, DraggingEllipse, DraggingRect, Idle)
  , presetBrush
  )
import Syntic.Domain.Brush
  ( Brush
  , brushFlow
  , brushOpacity
  , brushSize
  , brushSoftness
  )
import Syntic.Domain.BrushStroke
  ( BrushStroke
  , StrokeSample (samplePosition, samplePressure)
  , strokeBrush
  , strokeColor
  , strokeSamples
  )
import Syntic.Domain.Color (Color (alpha, blue, green, red))
import Syntic.Domain.Document
  ( Document
  , backgroundColor
  , canvasSize
  , documentLayers
  )
import Syntic.Domain.Geometry
  ( Point (pointX, pointY)
  , Rectangle (rectangleHeight, rectangleOrigin, rectangleWidth)
  , Ellipse (ellipseCenter, ellipseRadiusX, ellipseRadiusY)
  , canvasHeight
  , canvasWidth
  )
import Syntic.Domain.Layer
  ( BlendMode (Darken, Lighten, Multiply, Normal, Overlay, Screen)
  , Layer
  , layerBlendMode
  , layerOpacity
  , layerVisible
  , orderedLayerShapes
  )
import Syntic.Domain.Shape
  ( Shape (shapeGeometry, shapeStyle)
  , ShapeGeometry
    ( BrushStrokeGeometry
    , EllipseGeometry
    , PolylineGeometry
    , RectangleGeometry
    )
  , Stroke (strokeColor, strokeWidth)
  , Style (fillColor, stroke)
  )

-- | Render the given document to the current Cairo context.
renderDocument :: Document -> Cairo.Render ()
renderDocument doc = do
  let bg = backgroundColor doc
      w  = fromIntegral (canvasWidth  (canvasSize doc)) :: Double
      h  = fromIntegral (canvasHeight (canvasSize doc)) :: Double
  Cairo.setOperator Cairo.OperatorOver
  setSolid bg
  Cairo.rectangle 0 0 w h
  Cairo.fill
  mapM_ renderLayer (documentLayers doc)

-- | Render an in-progress drag (brush stroke, rubber-band rect/ellipse).
renderDragPreview :: AppState -> Cairo.Render ()
renderDragPreview st =
  case asDrag st of
    Idle -> pure ()
    BrushStroking samples ->
      case NE.nonEmpty (reverse samples) of
        Nothing -> pure ()
        Just ne -> renderBrushDots (presetBrush (asBrushPreset st)) (asColor st) (NE.toList ne)
    DraggingRect start current ->
      renderRubberBandRect start current
    DraggingEllipse start current ->
      renderRubberBandEllipse start current

-- Layers ------------------------------------------------------------------

renderLayer :: Layer -> Cairo.Render ()
renderLayer layer = when (layerVisible layer) $ do
  Cairo.save
  Cairo.pushGroup
  mapM_ renderShape (orderedLayerShapes layer)
  Cairo.popGroupToSource
  Cairo.setOperator (blendOperator (layerBlendMode layer))
  Cairo.paintWithAlpha (layerOpacity layer)
  Cairo.restore

blendOperator :: BlendMode -> Cairo.Operator
blendOperator mode = case mode of
  Normal   -> Cairo.OperatorOver
  Multiply -> Cairo.OperatorMultiply
  Screen   -> Cairo.OperatorScreen
  Overlay  -> Cairo.OperatorOverlay
  Darken   -> Cairo.OperatorDarken
  Lighten  -> Cairo.OperatorLighten

-- Shapes ------------------------------------------------------------------

renderShape :: Shape -> Cairo.Render ()
renderShape shape =
  case shapeGeometry shape of
    RectangleGeometry r ->
      renderRectShape r (shapeStyle shape)
    EllipseGeometry e ->
      renderEllipseShape e (shapeStyle shape)
    PolylineGeometry points ->
      renderPolylineShape (NE.toList points) (shapeStyle shape)
    BrushStrokeGeometry bs ->
      renderBrushStroke bs

renderRectShape :: Rectangle -> Style -> Cairo.Render ()
renderRectShape r style = do
  let Point x y = rectangleOrigin r
      w         = fromIntegral (rectangleWidth r)
      h         = fromIntegral (rectangleHeight r)
  Cairo.rectangle (fromIntegral x) (fromIntegral y) w h
  paintStyle style

renderEllipseShape :: Ellipse -> Style -> Cairo.Render ()
renderEllipseShape e style = do
  let Point cx cy = ellipseCenter e
      rx          = fromIntegral (ellipseRadiusX e) :: Double
      ry          = fromIntegral (ellipseRadiusY e) :: Double
  Cairo.save
  Cairo.translate (fromIntegral cx) (fromIntegral cy)
  Cairo.scale rx ry
  Cairo.arc 0 0 1 0 (2 * pi)
  Cairo.restore
  paintStyle style

renderPolylineShape :: [Point] -> Style -> Cairo.Render ()
renderPolylineShape [] _ = pure ()
renderPolylineShape (p : ps) style = do
  Cairo.moveTo (fromIntegral (pointX p)) (fromIntegral (pointY p))
  mapM_ (\q -> Cairo.lineTo (fromIntegral (pointX q)) (fromIntegral (pointY q))) ps
  paintStyle style

paintStyle :: Style -> Cairo.Render ()
paintStyle style = do
  case fillColor style of
    Just c -> do
      setSolid c
      Cairo.fillPreserve
    Nothing -> pure ()
  case stroke style of
    Just s -> do
      setSolid (strokeColor s)
      Cairo.setLineWidth (fromIntegral (strokeWidth s))
      Cairo.stroke
    Nothing ->
      Cairo.newPath

-- Brush strokes -----------------------------------------------------------

renderBrushStroke :: BrushStroke -> Cairo.Render ()
renderBrushStroke bs =
  renderBrushDots (strokeBrush bs) (strokeColor bs) (NE.toList (strokeSamples bs))

-- | Paint a list of stroke samples as overlapping soft watercolor dots.
renderBrushDots :: Brush -> Color -> [StrokeSample] -> Cairo.Render ()
renderBrushDots brush color samples = do
  let baseRadius = brushSize brush / 2
      softness   = brushSoftness brush
      flow       = brushFlow brush
      opacity    = brushOpacity brush
  forM_ samples $ \sample -> do
    let Point px py = samplePosition sample
        pressure   = samplePressure sample
        -- bigger when pressure is higher, slightly smaller when softer
        radius     = max 0.5 (baseRadius * (0.3 + 0.7 * pressure) * (1.0 + softness * 0.25))
        a          = clamp01 (opacity * flow * (0.15 + 0.85 * pressure))
    setColorA color a
    Cairo.arc (fromIntegral px) (fromIntegral py) radius 0 (2 * pi)
    Cairo.fill

-- Rubber-band previews ----------------------------------------------------

renderRubberBandRect :: Point -> Point -> Cairo.Render ()
renderRubberBandRect a b = do
  let (x, y, w, h) = normalizeBox a b
  Cairo.save
  Cairo.setSourceRGBA 0 0 0 0.6
  Cairo.setLineWidth 1
  Cairo.setDash [4, 3] 0
  Cairo.rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  Cairo.stroke
  Cairo.restore

renderRubberBandEllipse :: Point -> Point -> Cairo.Render ()
renderRubberBandEllipse a b = do
  let (x, y, w, h) = normalizeBox a b
      cx           = fromIntegral x + fromIntegral w / 2
      cy           = fromIntegral y + fromIntegral h / 2
      rx           = max 0.5 (fromIntegral w / 2)
      ry           = max 0.5 (fromIntegral h / 2)
  Cairo.save
  Cairo.setSourceRGBA 0 0 0 0.6
  Cairo.setLineWidth 1
  Cairo.setDash [4, 3] 0
  Cairo.translate cx cy
  Cairo.scale rx ry
  Cairo.arc 0 0 1 0 (2 * pi)
  Cairo.restore
  Cairo.stroke

normalizeBox :: Point -> Point -> (Int, Int, Int, Int)
normalizeBox (Point x1 y1) (Point x2 y2) =
  ( min x1 x2
  , min y1 y2
  , abs (x2 - x1)
  , abs (y2 - y1)
  )

-- Helpers -----------------------------------------------------------------

setSolid :: Color -> Cairo.Render ()
setSolid c =
  Cairo.setSourceRGBA
    (byte (red c))
    (byte (green c))
    (byte (blue c))
    (byte (alpha c))

setColorA :: Color -> Double -> Cairo.Render ()
setColorA c a =
  Cairo.setSourceRGBA
    (byte (red c))
    (byte (green c))
    (byte (blue c))
    a

byte :: Word8 -> Double
byte w = fromIntegral w / 255.0

clamp01 :: Double -> Double
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
