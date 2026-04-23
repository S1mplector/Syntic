module Syntic.Application.Editor
  ( EditorService (..)
  , CreateDocumentRequest (..)
  , ApplicationError (..)
  , mkEditorService
  ) where

import Data.Bifunctor (first)
import Syntic.Application.History
  ( currentDocument
  , newHistory
  , recordChange
  , redo
  , undo
  )
import Syntic.Application.Port.DocumentStore
  ( DocumentStore (lookupDocument, lookupHistory, saveDocument, saveHistory)
  )
import Syntic.Domain.Color (Color)
import Syntic.Domain.Document
  ( Document
  , DocumentCommand
  , DomainError
  , applyCommand
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
  | NothingToUndo !DocumentId
  | NothingToRedo !DocumentId
  deriving stock (Eq, Show)

data EditorService store = EditorService
  { createDocument        :: store -> CreateDocumentRequest -> Either ApplicationError (store, Document)
  , applyDocumentCommand  :: store -> DocumentId -> DocumentCommand -> Either ApplicationError (store, Document)
  , getDocument           :: store -> DocumentId -> Either ApplicationError Document
  , undoDocument          :: store -> DocumentId -> Either ApplicationError (store, Document)
  , redoDocument          :: store -> DocumentId -> Either ApplicationError (store, Document)
  }

mkEditorService :: DocumentStore store -> EditorService store
mkEditorService storePort =
  EditorService
    { createDocument       = createDocumentWith storePort
    , applyDocumentCommand = applyDocumentCommandWith storePort
    , getDocument          = getDocumentWith storePort
    , undoDocument         = undoDocumentWith storePort
    , redoDocument         = redoDocumentWith storePort
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
  current <- getDocumentWith storePort store targetDocumentId
  updatedDocument <- first InvalidDocumentChange (applyCommand command current)
  let history = case lookupHistory storePort store targetDocumentId of
        Just h  -> recordChange updatedDocument h
        Nothing -> recordChange updatedDocument (newHistory current)
      nextStore =
        saveHistory storePort
          (saveDocument storePort store updatedDocument)
          targetDocumentId
          history
  pure (nextStore, updatedDocument)

undoDocumentWith :: DocumentStore store -> store -> DocumentId -> Either ApplicationError (store, Document)
undoDocumentWith storePort store targetDocumentId = do
  _ <- getDocumentWith storePort store targetDocumentId
  case lookupHistory storePort store targetDocumentId >>= undo of
    Nothing -> Left (NothingToUndo targetDocumentId)
    Just updatedHistory ->
      let doc = currentDocument updatedHistory
          nextStore =
            saveHistory storePort
              (saveDocument storePort store doc)
              targetDocumentId
              updatedHistory
       in Right (nextStore, doc)

redoDocumentWith :: DocumentStore store -> store -> DocumentId -> Either ApplicationError (store, Document)
redoDocumentWith storePort store targetDocumentId = do
  _ <- getDocumentWith storePort store targetDocumentId
  case lookupHistory storePort store targetDocumentId >>= redo of
    Nothing -> Left (NothingToRedo targetDocumentId)
    Just updatedHistory ->
      let doc = currentDocument updatedHistory
          nextStore =
            saveHistory storePort
              (saveDocument storePort store doc)
              targetDocumentId
              updatedHistory
       in Right (nextStore, doc)

getDocumentWith :: DocumentStore store -> store -> DocumentId -> Either ApplicationError Document
getDocumentWith storePort store targetDocumentId =
  case lookupDocument storePort store targetDocumentId of
    Just document ->
      Right document
    Nothing ->
      Left (DocumentMissing targetDocumentId)

