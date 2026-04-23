module Syntic.Domain.Layer
  ( Layer
  , BlendMode (..)
  , LayerError (..)
  , emptyLayer
  , layerId
  , layerName
  , layerOpacity
  , layerBlendMode
  , layerVisible
  , addShapeToLayer
  , removeShapeFromLayer
  , updateShapeInLayer
  , lookupShapeInLayer
  , orderedLayerShapes
  , setLayerOpacity
  , setLayerBlendMode
  , setLayerVisibility
  , renameLayer
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Syntic.Domain.Identifier (LayerId, ShapeId)
import Syntic.Domain.Shape (Shape (..))

data BlendMode
  = Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  deriving stock (Eq, Show)

data Layer = Layer
  { internalLayerId       :: !LayerId
  , internalLayerName     :: !String
  , internalLayerOpacity  :: !Double
  , internalLayerBlend    :: !BlendMode
  , internalLayerVisible  :: !Bool
  , internalLayerShapes   :: !(Map ShapeId Shape)
  , internalLayerStacking :: ![ShapeId]
  }
  deriving stock (Eq, Show)

data LayerError
  = LayerShapeAlreadyExists !ShapeId
  | LayerShapeNotFound !ShapeId
  deriving stock (Eq, Show)

layerId :: Layer -> LayerId
layerId = internalLayerId

layerName :: Layer -> String
layerName = internalLayerName

layerOpacity :: Layer -> Double
layerOpacity = internalLayerOpacity

layerBlendMode :: Layer -> BlendMode
layerBlendMode = internalLayerBlend

layerVisible :: Layer -> Bool
layerVisible = internalLayerVisible

emptyLayer :: LayerId -> String -> Layer
emptyLayer newLayerId name =
  Layer
    { internalLayerId       = newLayerId
    , internalLayerName     = name
    , internalLayerOpacity  = 1.0
    , internalLayerBlend    = Normal
    , internalLayerVisible  = True
    , internalLayerShapes   = Map.empty
    , internalLayerStacking = []
    }

addShapeToLayer :: Shape -> Layer -> Either LayerError Layer
addShapeToLayer shape layer
  | Map.member sid (internalLayerShapes layer) =
      Left (LayerShapeAlreadyExists sid)
  | otherwise =
      Right
        layer
          { internalLayerShapes   = Map.insert sid shape (internalLayerShapes layer)
          , internalLayerStacking = internalLayerStacking layer <> [sid]
          }
  where
    sid = shapeId shape

removeShapeFromLayer :: ShapeId -> Layer -> Either LayerError Layer
removeShapeFromLayer targetId layer
  | Map.member targetId (internalLayerShapes layer) =
      Right
        layer
          { internalLayerShapes   = Map.delete targetId (internalLayerShapes layer)
          , internalLayerStacking = filter (/= targetId) (internalLayerStacking layer)
          }
  | otherwise =
      Left (LayerShapeNotFound targetId)

updateShapeInLayer :: ShapeId -> (Shape -> Shape) -> Layer -> Either LayerError Layer
updateShapeInLayer targetId transform layer =
  case Map.lookup targetId (internalLayerShapes layer) of
    Nothing ->
      Left (LayerShapeNotFound targetId)
    Just currentShape ->
      Right
        layer
          { internalLayerShapes =
              Map.insert targetId (transform currentShape) (internalLayerShapes layer)
          }

lookupShapeInLayer :: ShapeId -> Layer -> Maybe Shape
lookupShapeInLayer targetId layer =
  Map.lookup targetId (internalLayerShapes layer)

orderedLayerShapes :: Layer -> [Shape]
orderedLayerShapes layer =
  mapMaybe (`Map.lookup` internalLayerShapes layer) (internalLayerStacking layer)

setLayerOpacity :: Double -> Layer -> Layer
setLayerOpacity value layer = layer { internalLayerOpacity = value }

setLayerBlendMode :: BlendMode -> Layer -> Layer
setLayerBlendMode mode layer = layer { internalLayerBlend = mode }

setLayerVisibility :: Bool -> Layer -> Layer
setLayerVisibility vis layer = layer { internalLayerVisible = vis }

renameLayer :: String -> Layer -> Layer
renameLayer name layer = layer { internalLayerName = name }
