module Syntic.Application.Port.DocumentStore
  ( DocumentStore (..)
  ) where

import Syntic.Domain.Document (Document)
import Syntic.Domain.Identifier (DocumentId)

data DocumentStore store = DocumentStore
  { lookupDocument :: store -> DocumentId -> Maybe Document
  , saveDocument :: store -> Document -> store
  }

