module Main (main) where

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
  ( CreateDocumentRequest (..)
  , EditorService (..)
  , mkEditorService
  )
import Syntic.Domain.Color (opaque)
import Syntic.Domain.Document
  ( DocumentCommand (AddShape, MoveShape)
  , DomainError (ShapeAlreadyExists)
  , applyCommand
  , emptyDocument
  , orderedShapes
  )
import Syntic.Domain.Geometry
  ( Point (..)
  , Rectangle (..)
  , Vector (..)
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

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite =
  TestList
    [ TestLabel "document adds shapes in stacking order" testAddShape
    , TestLabel "document rejects duplicate shape identifiers" testRejectDuplicateShapeIds
    , TestLabel "editor persists changes through the document store" testEditorPersistsChanges
    ]

testAddShape :: Test
testAddShape =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-1") testCanvas (opaque 255 255 255)

    updatedDocument <- expectRight (applyCommand (AddShape sampleRectangle) document)
    orderedShapes updatedDocument @?= [sampleRectangle]

testRejectDuplicateShapeIds :: Test
testRejectDuplicateShapeIds =
  TestCase $ do
    testCanvas <- expectRight (mkCanvasSize 800 600)
    let document = emptyDocument (DocumentId "doc-1") testCanvas (opaque 255 255 255)

    documentWithShape <- expectRight (applyCommand (AddShape sampleRectangle) document)

    case applyCommand (AddShape sampleRectangle) documentWithShape of
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
    (storeAfterMove, _) <-
      expectRight
        ( applyDocumentCommand
            editor
            storeAfterCreate
            (DocumentId "doc-2")
            (AddShape sampleRectangle)
        )

    (storeAfterUpdate, _) <-
      expectRight
        ( applyDocumentCommand
            editor
            storeAfterMove
            (DocumentId "doc-2")
            (MoveShape (ShapeId "shape-1") (Vector 10 20))
        )

    loadedDocument <- expectRight (getDocument editor storeAfterUpdate (DocumentId "doc-2"))
    orderedShapes loadedDocument @?= [movedRectangle]

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

expectRight :: Show error => Either error value -> IO value
expectRight result =
  case result of
    Left inputError ->
      assertFailure ("expected a successful result, but received: " <> show inputError)
    Right value ->
      pure value
