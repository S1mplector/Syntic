module Syntic.Adapter.InMemory.DocumentStore
  ( InMemoryDocumentStore
  , emptyStore
  , documentStorePort
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Syntic.Application.Port.DocumentStore (DocumentStore (..))
import Syntic.Domain.Document (Document, documentId)
import Syntic.Domain.Identifier (DocumentId)

newtype InMemoryDocumentStore = InMemoryDocumentStore
  { internalDocuments :: Map DocumentId Document
  }
  deriving stock (Eq, Show)

emptyStore :: InMemoryDocumentStore
emptyStore = InMemoryDocumentStore Map.empty

documentStorePort :: DocumentStore InMemoryDocumentStore
documentStorePort =
  DocumentStore
    { lookupDocument = lookupDocumentInMemory
    , saveDocument = saveDocumentInMemory
    }

lookupDocumentInMemory :: InMemoryDocumentStore -> DocumentId -> Maybe Document
lookupDocumentInMemory (InMemoryDocumentStore documents) targetDocumentId =
  Map.lookup targetDocumentId documents

saveDocumentInMemory :: InMemoryDocumentStore -> Document -> InMemoryDocumentStore
saveDocumentInMemory (InMemoryDocumentStore documents) document =
  InMemoryDocumentStore (Map.insert (documentId document) document documents)

