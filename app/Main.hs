module Main (main) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Syntic.Adapter.InMemory.DocumentStore
  ( documentStorePort
  , emptyStore
  )
import Syntic.Application.Editor
  ( ApplicationError
  , CreateDocumentRequest (CreateDocumentRequest, createBackgroundColor, createCanvasSize, createDocumentId)
  , EditorService (applyDocumentCommand, createDocument, redoDocument, undoDocument)
  , mkEditorService
  )
import Syntic.Domain.Brush (defaultWatercolorBrush, washBrush)
import Syntic.Domain.BrushStroke (BrushStroke (BrushStroke, strokeBrush, strokeColor, strokeSamples), StrokeSample (StrokeSample))
import Syntic.Domain.Color (opaque)
import Syntic.Domain.Document
  ( Document
  , DocumentCommand (AddLayer, AddShape)
  , defaultLayerId
  , orderedShapes
  )
import Syntic.Domain.Geometry
  ( CanvasSizeError
  , Point (Point)
  , mkCanvasSize
  )
import Syntic.Domain.Identifier
  ( DocumentId (DocumentId)
  , LayerId (LayerId)
  , ShapeId (ShapeId)
  )
import Syntic.Domain.Shape
  ( Shape (Shape, shapeGeometry, shapeId, shapeStyle)
  , ShapeGeometry (BrushStrokeGeometry)
  , Style (Style, fillColor, stroke)
  )

main :: IO ()
main =
  case exampleScenario of
    Left exampleError ->
      putStrLn ("Error: " <> show exampleError)
    Right document -> do
      putStrLn "Syntic — watercolor demo"
      putStrLn ("Shapes on canvas: " <> show (length (orderedShapes document)))
      mapM_ (putStrLn . describeShape) (orderedShapes document)

describeShape :: Shape -> String
describeShape shape =
  case shapeGeometry shape of
    BrushStrokeGeometry bs ->
      "  brush stroke " <> show (shapeId shape) <> " — " <> show (length (strokeSamples bs)) <> " samples"
    other ->
      "  geometry " <> show (shapeId shape) <> " — " <> show other

data ExampleError
  = InvalidExampleCanvas !CanvasSizeError
  | EditorFailure !ApplicationError
  deriving stock (Eq, Show)

exampleScenario :: Either ExampleError Document
exampleScenario = do
  canvas <- first InvalidExampleCanvas (mkCanvasSize 1600 900)
  let editor  = mkEditorService documentStorePort
      request =
        CreateDocumentRequest
          { createDocumentId      = DocumentId "demo"
          , createCanvasSize      = canvas
          , createBackgroundColor = opaque 255 255 255
          }

  -- Create document (comes with a default layer)
  (s1, _) <- first EditorFailure (createDocument editor emptyStore request)

  -- Add a "Washes" layer for background washes
  (s2, _) <-
    first EditorFailure $
      applyDocumentCommand editor s1 (DocumentId "demo")
        (AddLayer (LayerId "washes") "Washes")

  -- Paint a wash stroke on the Washes layer
  (s3, _) <-
    first EditorFailure $
      applyDocumentCommand editor s2 (DocumentId "demo")
        (AddShape (LayerId "washes") skyWash)

  -- Paint a detail stroke on the default layer
  (s4, _) <-
    first EditorFailure $
      applyDocumentCommand editor s3 (DocumentId "demo")
        (AddShape defaultLayerId detailStroke)

  -- Undo the last stroke
  (s5, _) <-
    first EditorFailure $
      undoDocument editor s4 (DocumentId "demo")

  -- Redo it back
  (_, finalDoc) <-
    first EditorFailure $
      redoDocument editor s5 (DocumentId "demo")

  pure finalDoc

skyWash :: Shape
skyWash =
  Shape
    { shapeId = ShapeId "wash-1"
    , shapeGeometry =
        BrushStrokeGeometry
          BrushStroke
            { strokeBrush   = washBrush
            , strokeColor   = opaque 135 206 235
            , strokeSamples =
                StrokeSample (Point 50 50)  0.3
                  :| [ StrokeSample (Point 400 60)  0.5
                     , StrokeSample (Point 800 45)  0.6
                     , StrokeSample (Point 1200 55) 0.4
                     , StrokeSample (Point 1550 50) 0.3
                     ]
            }
    , shapeStyle = Style { fillColor = Nothing, stroke = Nothing }
    }

detailStroke :: Shape
detailStroke =
  Shape
    { shapeId = ShapeId "detail-1"
    , shapeGeometry =
        BrushStrokeGeometry
          BrushStroke
            { strokeBrush   = defaultWatercolorBrush
            , strokeColor   = opaque 34 80 50
            , strokeSamples =
                StrokeSample (Point 200 400) 0.7
                  :| [ StrokeSample (Point 220 380) 0.8
                     , StrokeSample (Point 260 370) 0.9
                     , StrokeSample (Point 310 375) 0.85
                     , StrokeSample (Point 350 390) 0.6
                     ]
            }
    , shapeStyle = Style { fillColor = Nothing, stroke = Nothing }
    }
