module Syntic.Domain.Document
  ( Document
  , DocumentCommand (..)
  , DomainError (..)
  , defaultLayerId
  , documentId
  , canvasSize
  , backgroundColor
  , emptyDocument
  , applyCommand
  , lookupShape
  , orderedShapes
  , documentLayers
  , lookupLayer
  ) where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Syntic.Domain.Color (Color)
import Syntic.Domain.Geometry (CanvasSize, Vector)
import Syntic.Domain.Identifier (DocumentId, LayerId (LayerId), ShapeId)
import Syntic.Domain.Layer
  ( BlendMode
  , Layer
  , LayerError (LayerShapeAlreadyExists, LayerShapeNotFound)
  , addShapeToLayer
  , emptyLayer
  , lookupShapeInLayer
  , orderedLayerShapes
  , removeShapeFromLayer
  , renameLayer
  , setLayerBlendMode
  , setLayerOpacity
  , setLayerVisibility
  , updateShapeInLayer
  )
import Syntic.Domain.Shape (Shape (shapeStyle), Style, moveShape)

data Document = Document
  { internalDocumentId      :: !DocumentId
  , internalCanvasSize      :: !CanvasSize
  , internalBackgroundColor :: !Color
  , internalLayers          :: !(Map LayerId Layer)
  , internalLayerOrder      :: ![LayerId]
  }
  deriving stock (Eq, Show)

data DocumentCommand
  = AddLayer !LayerId !String
  | RemoveLayer !LayerId
  | SetLayerOpacity !LayerId !Double
  | SetLayerBlendMode !LayerId !BlendMode
  | SetLayerVisibility !LayerId !Bool
  | RenameLayer !LayerId !String
  | AddShape !LayerId !Shape
  | MoveShape !LayerId !ShapeId !Vector
  | RestyleShape !LayerId !ShapeId !Style
  | RemoveShape !LayerId !ShapeId
  deriving stock (Eq, Show)

data DomainError
  = LayerAlreadyExists !LayerId
  | LayerNotFound !LayerId
  | ShapeAlreadyExists !ShapeId
  | ShapeNotFound !ShapeId
  deriving stock (Eq, Show)

defaultLayerId :: LayerId
defaultLayerId = LayerId "default"

documentId :: Document -> DocumentId
documentId = internalDocumentId

canvasSize :: Document -> CanvasSize
canvasSize = internalCanvasSize

backgroundColor :: Document -> Color
backgroundColor = internalBackgroundColor

documentLayers :: Document -> [Layer]
documentLayers document =
  foldr
    (\lid acc -> maybe acc (: acc) (Map.lookup lid (internalLayers document)))
    []
    (internalLayerOrder document)

lookupLayer :: LayerId -> Document -> Maybe Layer
lookupLayer lid = Map.lookup lid . internalLayers

emptyDocument :: DocumentId -> CanvasSize -> Color -> Document
emptyDocument newDocumentId newCanvasSize newBackgroundColor =
  Document
    { internalDocumentId      = newDocumentId
    , internalCanvasSize      = newCanvasSize
    , internalBackgroundColor = newBackgroundColor
    , internalLayers          = Map.singleton defaultLayerId (emptyLayer defaultLayerId "Background")
    , internalLayerOrder      = [defaultLayerId]
    }

applyCommand :: DocumentCommand -> Document -> Either DomainError Document
applyCommand command document =
  case command of
    AddLayer lid name ->
      addLayerToDocument lid name document
    RemoveLayer lid ->
      removeLayerFromDocument lid document
    SetLayerOpacity lid value ->
      modifyLayer lid (setLayerOpacity value) document
    SetLayerBlendMode lid mode ->
      modifyLayer lid (setLayerBlendMode mode) document
    SetLayerVisibility lid vis ->
      modifyLayer lid (setLayerVisibility vis) document
    RenameLayer lid name ->
      modifyLayer lid (renameLayer name) document
    AddShape lid shape ->
      withLayer lid document $ \layer ->
        first liftLayerError (addShapeToLayer shape layer)
    MoveShape lid sid offset ->
      withLayer lid document $ \layer ->
        first liftLayerError (updateShapeInLayer sid (moveShape offset) layer)
    RestyleShape lid sid nextStyle ->
      withLayer lid document $ \layer ->
        first liftLayerError (updateShapeInLayer sid (\s -> s {shapeStyle = nextStyle}) layer)
    RemoveShape lid sid ->
      withLayer lid document $ \layer ->
        first liftLayerError (removeShapeFromLayer sid layer)

lookupShape :: LayerId -> ShapeId -> Document -> Maybe Shape
lookupShape lid sid document = do
  layer <- Map.lookup lid (internalLayers document)
  lookupShapeInLayer sid layer

orderedShapes :: Document -> [Shape]
orderedShapes document =
  concatMap orderedLayerShapes (documentLayers document)

-- Internal helpers -------------------------------------------------------

addLayerToDocument :: LayerId -> String -> Document -> Either DomainError Document
addLayerToDocument lid name document
  | Map.member lid (internalLayers document) =
      Left (LayerAlreadyExists lid)
  | otherwise =
      Right
        document
          { internalLayers     = Map.insert lid (emptyLayer lid name) (internalLayers document)
          , internalLayerOrder = internalLayerOrder document <> [lid]
          }

removeLayerFromDocument :: LayerId -> Document -> Either DomainError Document
removeLayerFromDocument lid document
  | Map.member lid (internalLayers document) =
      Right
        document
          { internalLayers     = Map.delete lid (internalLayers document)
          , internalLayerOrder = filter (/= lid) (internalLayerOrder document)
          }
  | otherwise =
      Left (LayerNotFound lid)

modifyLayer :: LayerId -> (Layer -> Layer) -> Document -> Either DomainError Document
modifyLayer lid transform document =
  case Map.lookup lid (internalLayers document) of
    Nothing -> Left (LayerNotFound lid)
    Just layer ->
      Right document { internalLayers = Map.insert lid (transform layer) (internalLayers document) }

withLayer :: LayerId -> Document -> (Layer -> Either DomainError Layer) -> Either DomainError Document
withLayer lid document action =
  case Map.lookup lid (internalLayers document) of
    Nothing -> Left (LayerNotFound lid)
    Just layer -> do
      updatedLayer <- action layer
      pure document { internalLayers = Map.insert lid updatedLayer (internalLayers document) }

liftLayerError :: LayerError -> DomainError
liftLayerError (LayerShapeAlreadyExists sid) = ShapeAlreadyExists sid
liftLayerError (LayerShapeNotFound sid) = ShapeNotFound sid

