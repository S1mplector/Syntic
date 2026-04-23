module Main (main) where

import Data.Bifunctor (first)
import Syntic.Adapter.InMemory.DocumentStore
  ( documentStorePort
  , emptyStore
  )
import Syntic.Application.Editor
  ( ApplicationError
  , CreateDocumentRequest (..)
  , EditorService (..)
  , mkEditorService
  )
import Syntic.Domain.Color (opaque)
import Syntic.Domain.Document
  ( Document
  , DocumentCommand (AddShape)
  )
import Syntic.Domain.Geometry
  ( CanvasSizeError
  , Point (..)
  , Rectangle (..)
  , mkCanvasSize
  )
import Syntic.Domain.Identifier
  ( DocumentId (..)
  , ShapeId (..)
  )
import Syntic.Domain.Shape
  ( Shape (..)
  , ShapeGeometry (RectangleGeometry)
  , Style (..)
  )

main :: IO ()
main =
  case exampleScenario of
    Left exampleError ->
      print exampleError
    Right document ->
      print document

data ExampleError
  = InvalidExampleCanvas !CanvasSizeError
  | EditorFailure !ApplicationError
  deriving stock (Eq, Show)

exampleScenario :: Either ExampleError Document
exampleScenario = do
  canvas <- first InvalidExampleCanvas (mkCanvasSize 1600 900)
  let editor = mkEditorService documentStorePort
      request =
        CreateDocumentRequest
          { createDocumentId = DocumentId "demo"
          , createCanvasSize = canvas
          , createBackgroundColor = opaque 255 255 255
          }

  (storeAfterCreate, _) <- first EditorFailure (createDocument editor emptyStore request)
  (_, updatedDocument) <-
    first EditorFailure $
      applyDocumentCommand
        editor
        storeAfterCreate
        (DocumentId "demo")
        (AddShape sampleRectangle)

  pure updatedDocument

sampleRectangle :: Shape
sampleRectangle =
  Shape
    { shapeId = ShapeId "shape-1"
    , shapeGeometry =
        RectangleGeometry
          Rectangle
            { rectangleOrigin = Point 80 120
            , rectangleWidth = 320
            , rectangleHeight = 180
            }
    , shapeStyle =
        Style
          { fillColor = Just (opaque 255 180 0)
          , stroke = Nothing
          }
    }
