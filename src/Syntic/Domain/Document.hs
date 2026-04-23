module Syntic.Domain.Document
  ( Document
  , DocumentCommand (..)
  , DomainError (..)
  , documentId
  , canvasSize
  , backgroundColor
  , emptyDocument
  , applyCommand
  , lookupShape
  , orderedShapes
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Syntic.Domain.Color (Color)
import Syntic.Domain.Geometry (CanvasSize, Vector)
import Syntic.Domain.Identifier (DocumentId, ShapeId)
import Syntic.Domain.Shape (Shape (..), Style, moveShape)

data Document = Document
  { internalDocumentId :: !DocumentId
  , internalCanvasSize :: !CanvasSize
  , internalBackgroundColor :: !Color
  , internalShapes :: !(Map ShapeId Shape)
  , internalStackingOrder :: ![ShapeId]
  }
  deriving stock (Eq, Show)

data DocumentCommand
  = AddShape !Shape
  | MoveShape !ShapeId !Vector
  | RestyleShape !ShapeId !Style
  | RemoveShape !ShapeId
  deriving stock (Eq, Show)

data DomainError
  = ShapeAlreadyExists !ShapeId
  | ShapeNotFound !ShapeId
  deriving stock (Eq, Show)

documentId :: Document -> DocumentId
documentId = internalDocumentId

canvasSize :: Document -> CanvasSize
canvasSize = internalCanvasSize

backgroundColor :: Document -> Color
backgroundColor = internalBackgroundColor

emptyDocument :: DocumentId -> CanvasSize -> Color -> Document
emptyDocument newDocumentId newCanvasSize newBackgroundColor =
  Document
    { internalDocumentId = newDocumentId
    , internalCanvasSize = newCanvasSize
    , internalBackgroundColor = newBackgroundColor
    , internalShapes = Map.empty
    , internalStackingOrder = []
    }

applyCommand :: DocumentCommand -> Document -> Either DomainError Document
applyCommand command document =
  case command of
    AddShape shape ->
      addShape shape document
    MoveShape targetShapeId offset ->
      updateShape targetShapeId (moveShape offset) document
    RestyleShape targetShapeId nextStyle ->
      updateShape targetShapeId (\shape -> shape {shapeStyle = nextStyle}) document
    RemoveShape targetShapeId ->
      removeShape targetShapeId document

lookupShape :: ShapeId -> Document -> Maybe Shape
lookupShape targetShapeId document =
  Map.lookup targetShapeId (internalShapes document)

orderedShapes :: Document -> [Shape]
orderedShapes document =
  mapMaybe (`Map.lookup` internalShapes document) (internalStackingOrder document)

addShape :: Shape -> Document -> Either DomainError Document
addShape shape document
  | Map.member newShapeId (internalShapes document) =
      Left (ShapeAlreadyExists newShapeId)
  | otherwise =
      Right
        document
          { internalShapes = Map.insert newShapeId shape (internalShapes document)
          , internalStackingOrder = internalStackingOrder document <> [newShapeId]
          }
  where
    newShapeId = shapeId shape

updateShape :: ShapeId -> (Shape -> Shape) -> Document -> Either DomainError Document
updateShape targetShapeId transform document =
  case Map.lookup targetShapeId (internalShapes document) of
    Nothing ->
      Left (ShapeNotFound targetShapeId)
    Just currentShape ->
      Right
        document
          { internalShapes =
              Map.insert
                targetShapeId
                (transform currentShape)
                (internalShapes document)
          }

removeShape :: ShapeId -> Document -> Either DomainError Document
removeShape targetShapeId document
  | Map.member targetShapeId (internalShapes document) =
      Right
        document
          { internalShapes = Map.delete targetShapeId (internalShapes document)
          , internalStackingOrder = filter (/= targetShapeId) (internalStackingOrder document)
          }
  | otherwise =
      Left (ShapeNotFound targetShapeId)

