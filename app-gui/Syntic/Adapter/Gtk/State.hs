module Syntic.Adapter.Gtk.State
  ( AppState (..)
  , Tool (..)
  , DragState (..)
  , BrushPreset (..)
  , initialAppState
  , freshShapeId
  , presetBrush
  ) where

import Syntic.Adapter.InMemory.DocumentStore (InMemoryDocumentStore)
import Syntic.Domain.Brush
  ( Brush
  , defaultWatercolorBrush
  , detailBrush
  , washBrush
  )
import Syntic.Domain.BrushStroke (StrokeSample)
import Syntic.Domain.Color (Color, opaque)
import Syntic.Domain.Geometry (Point)
import Syntic.Domain.Identifier
  ( DocumentId
  , LayerId
  , ShapeId (ShapeId)
  )

data Tool
  = BrushTool
  | RectangleTool
  | EllipseTool
  deriving stock (Eq, Show)

data BrushPreset
  = DefaultBrush
  | WashBrush
  | DetailBrush
  deriving stock (Eq, Show)

data DragState
  = Idle
  | BrushStroking ![StrokeSample]
  | DraggingRect !Point !Point
  | DraggingEllipse !Point !Point
  deriving stock (Eq, Show)

data AppState = AppState
  { asStore        :: !InMemoryDocumentStore
  , asDocumentId   :: !DocumentId
  , asTool         :: !Tool
  , asActiveLayer  :: !LayerId
  , asBrushPreset  :: !BrushPreset
  , asColor        :: !Color
  , asDrag         :: !DragState
  , asShapeCounter :: !Int
  , asStatus       :: !String
  }

initialAppState
  :: InMemoryDocumentStore
  -> DocumentId
  -> LayerId
  -> AppState
initialAppState store docId lid =
  AppState
    { asStore        = store
    , asDocumentId   = docId
    , asTool         = BrushTool
    , asActiveLayer  = lid
    , asBrushPreset  = DefaultBrush
    , asColor        = opaque 30 60 140
    , asDrag         = Idle
    , asShapeCounter = 0
    , asStatus       = "Ready"
    }

freshShapeId :: AppState -> (AppState, ShapeId)
freshShapeId st =
  let n   = asShapeCounter st + 1
      sid = ShapeId ("shape-" <> show n)
   in (st { asShapeCounter = n }, sid)

presetBrush :: BrushPreset -> Brush
presetBrush DefaultBrush = defaultWatercolorBrush
presetBrush WashBrush    = washBrush
presetBrush DetailBrush  = detailBrush
