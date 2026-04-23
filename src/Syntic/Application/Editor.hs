module Syntic.Application.Editor
  ( EditorService (..)
  , CreateDocumentRequest (..)
  , ApplicationError (..)
  , mkEditorService
  ) where

import Data.Bifunctor (first)
import Syntic.Application.Port.DocumentStore (DocumentStore (..))
import Syntic.Domain.Color (Color)
import Syntic.Domain.Document
  ( Document
  , DocumentCommand
  , DomainError
  , applyCommand
  , documentId
  , emptyDocument
  )
import Syntic.Domain.Geometry (CanvasSize)
import Syntic.Domain.Identifier (DocumentId)

data CreateDocumentRequest = CreateDocumentRequest
  { createDocumentId :: !DocumentId
  , createCanvasSize :: !CanvasSize
  , createBackgroundColor :: !Color
  }
  deriving stock (Eq, Show)

data ApplicationError
  = DocumentAlreadyExists !DocumentId
  | DocumentMissing !DocumentId
  | InvalidDocumentChange !DomainError
  deriving stock (Eq, Show)

data EditorService store = EditorService
  { createDocument :: store -> CreateDocumentRequest -> Either ApplicationError (store, Document)
  , applyDocumentCommand :: store -> DocumentId -> DocumentCommand -> Either ApplicationError (store, Document)
  , getDocument :: store -> DocumentId -> Either ApplicationError Document
  }

mkEditorService :: DocumentStore store -> EditorService store
mkEditorService storePort =
  EditorService
    { createDocument = createDocumentWith storePort
    , applyDocumentCommand = applyDocumentCommandWith storePort
    , getDocument = getDocumentWith storePort
    }

createDocumentWith :: DocumentStore store -> store -> CreateDocumentRequest -> Either ApplicationError (store, Document)
createDocumentWith storePort store request =
  case lookupDocument storePort store (createDocumentId request) of
    Just _ ->
      Left (DocumentAlreadyExists (createDocumentId request))
    Nothing ->
      let document =
            emptyDocument
              (createDocumentId request)
              (createCanvasSize request)
              (createBackgroundColor request)
          nextStore = saveDocument storePort store document
       in Right (nextStore, document)

applyDocumentCommandWith :: DocumentStore store -> store -> DocumentId -> DocumentCommand -> Either ApplicationError (store, Document)
applyDocumentCommandWith storePort store targetDocumentId command = do
  currentDocument <- getDocumentWith storePort store targetDocumentId
  updatedDocument <- first InvalidDocumentChange (applyCommand command currentDocument)
  let nextStore = saveDocument storePort store updatedDocument
  pure (nextStore, updatedDocument)

getDocumentWith :: DocumentStore store -> store -> DocumentId -> Either ApplicationError Document
getDocumentWith storePort store targetDocumentId =
  case lookupDocument storePort store targetDocumentId of
    Just document ->
      Right document
    Nothing ->
      Left (DocumentMissing targetDocumentId)

