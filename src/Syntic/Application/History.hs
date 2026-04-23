module Syntic.Application.History
  ( History
  , newHistory
  , currentDocument
  , recordChange
  , undo
  , redo
  , canUndo
  , canRedo
  ) where

import Syntic.Domain.Document (Document)

data History = History
  { undoStack :: ![Document]
  , present   :: !Document
  , redoStack :: ![Document]
  }
  deriving stock (Eq, Show)

newHistory :: Document -> History
newHistory document =
  History
    { undoStack = []
    , present   = document
    , redoStack = []
    }

currentDocument :: History -> Document
currentDocument = present

recordChange :: Document -> History -> History
recordChange newDocument history =
  History
    { undoStack = present history : undoStack history
    , present   = newDocument
    , redoStack = []
    }

undo :: History -> Maybe History
undo history =
  case undoStack history of
    [] -> Nothing
    (prev : rest) ->
      Just
        History
          { undoStack = rest
          , present   = prev
          , redoStack = present history : redoStack history
          }

redo :: History -> Maybe History
redo history =
  case redoStack history of
    [] -> Nothing
    (next : rest) ->
      Just
        History
          { undoStack = present history : undoStack history
          , present   = next
          , redoStack = rest
          }

canUndo :: History -> Bool
canUndo = not . null . undoStack

canRedo :: History -> Bool
canRedo = not . null . redoStack
