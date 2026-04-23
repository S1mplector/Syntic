module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.HUnit
  ( Counts
  , Test (TestCase, TestLabel, TestList)
  , assertFailure
  , runTestTT
  , (@?=)
  )
import Syntic.Adapter.InMemory.DocumentStore
  ( documentStorePort
  , emptyStore
  )
import Syntic.Application.Editor
  ( ApplicationError (NothingToUndo)
  , CreateDocumentRequest (CreateDocumentRequest, createBackgroundColor, createCanvasSize, createDocumentId)
  , EditorService (applyDocumentCommand, createDocument, getDocument, redoDocument, undoDocument)
  , mkEditorService
  )
import Syntic.Domain.Brush (defaultWatercolorBrush)
import Syntic.Domain.BrushStroke (BrushStroke (BrushStroke, strokeBrush, strokeColor, strokeSamples), StrokeSample (StrokeSample))
import Syntic.Domain.Color (opaque)
import Syntic.Domain.Document
  ( DocumentCommand (AddLayer, AddShape, MoveShape)
  , DomainError (LayerAlreadyExists, ShapeAlreadyExists)
  , applyCommand
  , defaultLayerId
  , emptyDocument
  , orderedShapes
  )
import Syntic.Domain.Geometry
  ( Point (Point)
  , Rectangle (Rectangle, rectangleHeight, rectangleOrigin, rectangleWidth)
  , Vector (Vector)
  , mkCanvasSize
  )
import Syntic.Domain.Identifier
  ( DocumentId (DocumentId)
  , LayerId (LayerId)
  , ShapeId (ShapeId)
  )
import Syntic.Domain.Shape
  ( Shape (Shape, shapeGeometry, shapeId, shapeStyle)
  , ShapeGeometry (BrushStrokeGeometry, RectangleGeometry)
  , Style (Style, fillColor, stroke)
  )

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite =
  TestList
    [ TestLabel "document adds shapes to default layer" testAddShape
    , TestLabel "document rejects duplicate shape identifiers" testRejectDuplicateShapeIds
    , TestLabel "editor persists changes through the document store" testEditorPersistsChanges
    , TestLabel "add and use a custom layer" testCustomLayer
    , TestLabel "reject duplicate layer identifiers" testRejectDuplicateLayer
    , TestLabel "brush stroke shape can be added and moved" testBrushStroke
    , TestLabel "undo reverts the last command" testUndoRedo
    , TestLabel "undo on fresh document fails gracefully" testUndoOnFresh
    ]

testAddShape :: Test
testAddShape =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-1") testCanvas (opaque 255 255 255)

    updatedDocument <- expectRight (applyCommand (AddShape defaultLayerId sampleRectangle) document)
    orderedShapes updatedDocument @?= [sampleRectangle]

testRejectDuplicateShapeIds :: Test
testRejectDuplicateShapeIds =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-1") testCanvas (opaque 255 255 255)

    documentWithShape <- expectRight (applyCommand (AddShape defaultLayerId sampleRectangle) document)

    case applyCommand (AddShape defaultLayerId sampleRectangle) documentWithShape of
      Left (ShapeAlreadyExists duplicateShapeId) ->
        duplicateShapeId @?= ShapeId "shape-1"
      Left unexpectedError ->
        assertFailure ("unexpected domain error: " <> show unexpectedError)
      Right _ ->
        assertFailure "expected the duplicate shape identifier to be rejected"

testEditorPersistsChanges :: Test
testEditorPersistsChanges =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let editor = mkEditorService documentStorePort
        request =
          CreateDocumentRequest
            { createDocumentId = DocumentId "doc-2"
            , createCanvasSize = testCanvas
            , createBackgroundColor = opaque 255 255 255
            }

    (storeAfterCreate, _) <- expectRight (createDocument editor emptyStore request)
    (storeAfterAdd, _) <-
      expectRight
        ( applyDocumentCommand
            editor
            storeAfterCreate
            (DocumentId "doc-2")
            (AddShape defaultLayerId sampleRectangle)
        )

    (storeAfterUpdate, _) <-
      expectRight
        ( applyDocumentCommand
            editor
            storeAfterAdd
            (DocumentId "doc-2")
            (MoveShape defaultLayerId (ShapeId "shape-1") (Vector 10 20))
        )

    loadedDocument <- expectRight (getDocument editor storeAfterUpdate (DocumentId "doc-2"))
    orderedShapes loadedDocument @?= [movedRectangle]

testCustomLayer :: Test
testCustomLayer =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-3") testCanvas (opaque 255 255 255)
        washLayerId = LayerId "washes"

    withLayer <- expectRight (applyCommand (AddLayer washLayerId "Washes") document)
    withShape <- expectRight (applyCommand (AddShape washLayerId sampleRectangle) withLayer)
    orderedShapes withShape @?= [sampleRectangle]

testRejectDuplicateLayer :: Test
testRejectDuplicateLayer =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-4") testCanvas (opaque 255 255 255)

    case applyCommand (AddLayer defaultLayerId "Duplicate") document of
      Left (LayerAlreadyExists lid) ->
        lid @?= defaultLayerId
      Left unexpectedError ->
        assertFailure ("unexpected domain error: " <> show unexpectedError)
      Right _ ->
        assertFailure "expected duplicate layer to be rejected"

testBrushStroke :: Test
testBrushStroke =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-5") testCanvas (opaque 255 255 255)

    withStroke <- expectRight (applyCommand (AddShape defaultLayerId sampleBrushStroke) document)
    orderedShapes withStroke @?= [sampleBrushStroke]

    moved <- expectRight (applyCommand (MoveShape defaultLayerId (ShapeId "stroke-1") (Vector 5 10)) withStroke)
    orderedShapes moved @?= [movedBrushStroke]

testUndoRedo :: Test
testUndoRedo =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let editor = mkEditorService documentStorePort
        request =
          CreateDocumentRequest
            { createDocumentId = DocumentId "doc-6"
            , createCanvasSize = testCanvas
            , createBackgroundColor = opaque 255 255 255
            }

    (s1, _) <- expectRight (createDocument editor emptyStore request)
    (s2, _) <-
      expectRight
        ( applyDocumentCommand editor s1 (DocumentId "doc-6")
            (AddShape defaultLayerId sampleRectangle)
        )

    -- After add, document has one shape
    doc2 <- expectRight (getDocument editor s2 (DocumentId "doc-6"))
    orderedShapes doc2 @?= [sampleRectangle]

    -- Undo should revert to empty
    (s3, undoneDoc) <- expectRight (undoDocument editor s2 (DocumentId "doc-6"))
    orderedShapes undoneDoc @?= []

    -- Redo should bring shape back
    (_, redoneDoc) <- expectRight (redoDocument editor s3 (DocumentId "doc-6"))
    orderedShapes redoneDoc @?= [sampleRectangle]

testUndoOnFresh :: Test
testUndoOnFresh =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let editor = mkEditorService documentStorePort
        request =
          CreateDocumentRequest
            { createDocumentId = DocumentId "doc-7"
            , createCanvasSize = testCanvas
            , createBackgroundColor = opaque 255 255 255
            }

    (s1, _) <- expectRight (createDocument editor emptyStore request)
    case undoDocument editor s1 (DocumentId "doc-7") of
      Left (NothingToUndo _) -> pure ()
      Left unexpectedError ->
        assertFailure ("unexpected error: " <> show unexpectedError)
      Right _ ->
        assertFailure "expected NothingToUndo on a fresh document"

-- Fixtures ---------------------------------------------------------------

sampleRectangle :: Shape
sampleRectangle =
  Shape
    { shapeId = ShapeId "shape-1"
    , shapeGeometry =
        RectangleGeometry
          Rectangle
            { rectangleOrigin = Point 10 20
            , rectangleWidth = 100
            , rectangleHeight = 60
            }
    , shapeStyle =
        Style
          { fillColor = Just (opaque 200 50 50)
          , stroke = Nothing
          }
    }

movedRectangle :: Shape
movedRectangle =
  Shape
    { shapeId = ShapeId "shape-1"
    , shapeGeometry =
        RectangleGeometry
          Rectangle
            { rectangleOrigin = Point 20 40
            , rectangleWidth = 100
            , rectangleHeight = 60
            }
    , shapeStyle =
        Style
          { fillColor = Just (opaque 200 50 50)
          , stroke = Nothing
          }
    }

sampleBrushStroke :: Shape
sampleBrushStroke =
  Shape
    { shapeId = ShapeId "stroke-1"
    , shapeGeometry =
        BrushStrokeGeometry
          BrushStroke
            { strokeBrush   = defaultWatercolorBrush
            , strokeColor   = opaque 50 120 180
            , strokeSamples =
                StrokeSample (Point 100 200) 0.5
                  :| [StrokeSample (Point 150 210) 0.8]
            }
    , shapeStyle = Style { fillColor = Nothing, stroke = Nothing }
    }

movedBrushStroke :: Shape
movedBrushStroke =
  Shape
    { shapeId = ShapeId "stroke-1"
    , shapeGeometry =
        BrushStrokeGeometry
          BrushStroke
            { strokeBrush   = defaultWatercolorBrush
            , strokeColor   = opaque 50 120 180
            , strokeSamples =
                StrokeSample (Point 105 210) 0.5
                  :| [StrokeSample (Point 155 220) 0.8]
            }
    , shapeStyle = Style { fillColor = Nothing, stroke = Nothing }
    }

expectRight :: Show error => Either error value -> IO value
expectRight result =
  case result of
    Left inputError ->
      assertFailure ("expected a successful result, but received: " <> show inputError)
    Right value ->
      pure value
