module Syntic.Adapter.InMemory.DocumentStore
  ( InMemoryDocumentStore
  , emptyStore
  , documentStorePort
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Syntic.Application.History (History)
import Syntic.Application.Port.DocumentStore (DocumentStore (..))
import Syntic.Domain.Document (Document, documentId)
import Syntic.Domain.Identifier (DocumentId)

data InMemoryDocumentStore = InMemoryDocumentStore
  { internalDocuments  :: !(Map DocumentId Document)
  , internalHistories  :: !(Map DocumentId History)
  }
  deriving stock (Eq, Show)

emptyStore :: InMemoryDocumentStore
emptyStore = InMemoryDocumentStore Map.empty Map.empty

documentStorePort :: DocumentStore InMemoryDocumentStore
documentStorePort =
  DocumentStore
    { lookupDocument = lookupDocumentInMemory
    , saveDocument   = saveDocumentInMemory
    , lookupHistory  = lookupHistoryInMemory
    , saveHistory    = saveHistoryInMemory
    }

lookupDocumentInMemory :: InMemoryDocumentStore -> DocumentId -> Maybe Document
lookupDocumentInMemory store targetDocumentId =
  Map.lookup targetDocumentId (internalDocuments store)

saveDocumentInMemory :: InMemoryDocumentStore -> Document -> InMemoryDocumentStore
saveDocumentInMemory store document =
  store { internalDocuments = Map.insert (documentId document) document (internalDocuments store) }

lookupHistoryInMemory :: InMemoryDocumentStore -> DocumentId -> Maybe History
lookupHistoryInMemory store targetDocumentId =
  Map.lookup targetDocumentId (internalHistories store)

saveHistoryInMemory :: InMemoryDocumentStore -> DocumentId -> History -> InMemoryDocumentStore
saveHistoryInMemory store targetDocumentId history =
  store { internalHistories = Map.insert targetDocumentId history (internalHistories store) }

