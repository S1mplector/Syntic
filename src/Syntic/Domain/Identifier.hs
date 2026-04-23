module Syntic.Domain.Identifier
  ( DocumentId (..)
  , ShapeId (..)
  , LayerId (..)
  ) where

newtype DocumentId = DocumentId
  { unDocumentId :: String
  }
  deriving stock (Eq, Ord, Show)

newtype ShapeId = ShapeId
  { unShapeId :: String
  }
  deriving stock (Eq, Ord, Show)

newtype LayerId = LayerId
  { unLayerId :: String
  }
  deriving stock (Eq, Ord, Show)

