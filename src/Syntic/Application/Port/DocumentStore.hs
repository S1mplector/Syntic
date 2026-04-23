module Syntic.Application.Port.DocumentStore
  ( DocumentStore (..)
  ) where

import Syntic.Application.History (History)
import Syntic.Domain.Document (Document)
import Syntic.Domain.Identifier (DocumentId)

data DocumentStore store = DocumentStore
  { lookupDocument :: store -> DocumentId -> Maybe Document
  , saveDocument   :: store -> Document -> store
  , lookupHistory  :: store -> DocumentId -> Maybe History
  , saveHistory    :: store -> DocumentId -> History -> store
  }

