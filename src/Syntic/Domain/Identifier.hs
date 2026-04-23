module Syntic.Domain.Identifier
  ( DocumentId (..)
  , ShapeId (..)
  ) where

newtype DocumentId = DocumentId
  { unDocumentId :: String
  }
  deriving stock (Eq, Ord, Show)

newtype ShapeId = ShapeId
  { unShapeId :: String
  }
  deriving stock (Eq, Ord, Show)

