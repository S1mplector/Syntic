module Syntic.Adapter.Gtk.App
  ( runGui
  ) where

import Control.Monad (forM_, void, when)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.GI.Base
  ( AttrOp ((:=))
  , get
  , new
  , on
  , set
  )
import qualified GI.Cairo.Render.Connector as CairoGtk
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Syntic.Adapter.Gtk.Render (renderDocument, renderDragPreview)
import Syntic.Adapter.Gtk.State
  ( AppState
    ( asActiveLayer
    , asBrushPreset
    , asColor
    , asDocumentId
    , asDrag
    , asStatus
    , asStore
    , asTool
    )
  , BrushPreset (DefaultBrush, DetailBrush, WashBrush)
  , DragState (BrushStroking, DraggingEllipse, DraggingRect, Idle)
  , Tool (BrushTool, EllipseTool, RectangleTool)
  , freshShapeId
  , initialAppState
  , presetBrush
  )
import Syntic.Adapter.InMemory.DocumentStore
  ( documentStorePort
  , emptyStore
  )
import Syntic.Application.Editor
  ( ApplicationError
  , CreateDocumentRequest
    ( CreateDocumentRequest
    , createBackgroundColor
    , createCanvasSize
    , createDocumentId
    )
  , EditorService
    ( applyDocumentCommand
    , createDocument
    , getDocument
    , redoDocument
    , undoDocument
    )
  , mkEditorService
  )
import Syntic.Domain.BrushStroke
  ( BrushStroke (BrushStroke, strokeBrush, strokeColor, strokeSamples)
  , StrokeSample (StrokeSample, samplePosition, samplePressure)
  )
import Syntic.Domain.Color (opaque)
import Syntic.Domain.Document
  ( Document
  , DocumentCommand (AddLayer, AddShape, RemoveLayer)
  , defaultLayerId
  , documentLayers
  , emptyDocument
  )
import Syntic.Domain.Geometry
  ( CanvasSize
  , Point (Point)
  , Rectangle (Rectangle, rectangleHeight, rectangleOrigin, rectangleWidth)
  , Ellipse (Ellipse, ellipseCenter, ellipseRadiusX, ellipseRadiusY)
  , canvasHeight
  , canvasWidth
  , mkCanvasSize
  )
import Syntic.Domain.Identifier
  ( DocumentId (DocumentId)
  , LayerId (LayerId, unLayerId)
  )
import Syntic.Domain.Layer (layerId, layerName)
import Syntic.Domain.Shape
  ( Shape (Shape, shapeGeometry, shapeId, shapeStyle)
  , ShapeGeometry (BrushStrokeGeometry, EllipseGeometry, RectangleGeometry)
  , Style (Style, fillColor, stroke)
  )

-- | Launch the GTK editor. Blocks until the user closes the window.
runGui :: IO ()
runGui = do
  _ <- Gtk.init Nothing

  -- Build an initial 800x600 canvas document.
  cs <- case mkCanvasSize 800 600 of
    Right s  -> pure s
    Left err -> error ("Invalid canvas size: " <> show err)

  let editor = mkEditorService documentStorePort
      docId  = DocumentId "gui-document"
      req    = CreateDocumentRequest
                 { createDocumentId      = docId
                 , createCanvasSize      = cs
                 , createBackgroundColor = opaque 252 250 246
                 }

  (store0, _) <- case createDocument editor emptyStore req of
    Right ok  -> pure ok
    Left err  -> error ("Failed to create document: " <> show err)

  stateRef <- newIORef (initialAppState store0 docId defaultLayerId)

  -- Top-level window ------------------------------------------------------
  window <- new Gtk.Window
    [ #title         := "Syntic — Watercolor Editor"
    , #defaultWidth  := 1100
    , #defaultHeight := 760
    ]
  _ <- on window #destroy Gtk.mainQuit

  -- Root VBox -------------------------------------------------------------
  rootBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing     := 0
    ]
  #add window rootBox

  -- Menu bar --------------------------------------------------------------
  menuBar <- buildMenuBar stateRef editor
  #packStart rootBox menuBar False False 0

  -- Toolbar ---------------------------------------------------------------
  (toolBar, _, _, _) <- buildToolbar stateRef editor
  #packStart rootBox toolBar False False 0

  -- Main horizontal pane (canvas + side panel) ----------------------------
  mainBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing     := 4
    ]
  #packStart rootBox mainBox True True 0

  -- Canvas wrapped in a scrolled window + frame --------------------------
  canvasFrame <- new Gtk.Frame
    [ #shadowType := Gtk.ShadowTypeIn
    ]

  canvas <- new Gtk.DrawingArea []
  #setSizeRequest canvas
    (fromIntegral (canvasWidth cs))
    (fromIntegral (canvasHeight cs))
  #addEvents canvas
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskButtonReleaseMask
    , Gdk.EventMaskPointerMotionMask
    ]

  scroll <- new Gtk.ScrolledWindow []
  #add scroll canvas
  #add canvasFrame scroll
  #packStart mainBox canvasFrame True True 0

  -- Side panel ------------------------------------------------------------
  (sidePanel, layerList, rebuildLayers) <-
    buildSidePanel stateRef editor canvas
  #packStart mainBox sidePanel False False 0

  -- Status bar ------------------------------------------------------------
  statusBar <- new Gtk.Statusbar []
  #packStart rootBox statusBar False False 0
  ctx <- #getContextId statusBar ("main" :: Text)
  let setStatus msg = do
        _ <- #pop statusBar ctx
        _ <- #push statusBar ctx (T.pack msg)
        pure ()
  setStatus "Ready — Brush tool"

  -- Canvas draw signal ----------------------------------------------------
  _ <- on canvas #draw $ \cairoCtx -> do
    st  <- readIORef stateRef
    doc <- case getDocument editor (asStore st) (asDocumentId st) of
      Right d -> pure d
      Left  _ -> pure (emptyDoc cs)
    CairoGtk.renderWithContext
      (do
         renderDocument doc
         renderDragPreview st)
      cairoCtx
    pure True

  -- Canvas input handlers -------------------------------------------------
  _ <- on canvas #buttonPressEvent $ \event -> do
    btn <- get event #button
    when (btn == 1) $ do
      x <- get event #x
      y <- get event #y
      handlePress stateRef (round x) (round y)
      #queueDraw canvas
    pure True

  _ <- on canvas #motionNotifyEvent $ \event -> do
    x <- get event #x
    y <- get event #y
    changed <- handleMotion stateRef (round x) (round y)
    when changed (#queueDraw canvas)
    pure True

  _ <- on canvas #buttonReleaseEvent $ \event -> do
    btn <- get event #button
    when (btn == 1) $ do
      x <- get event #x
      y <- get event #y
      result <- handleRelease stateRef editor (round x) (round y)
      case result of
        Left err -> setStatus ("Error: " <> show err)
        Right msg -> setStatus msg
      rebuildLayers
      #queueDraw canvas
    pure True

  -- Initial population of the layer list ----------------------------------
  rebuildLayers

  -- Show the whole window -------------------------------------------------
  #showAll window
  Gtk.main

  -- silence unused-warnings on layerList for future selection-handling.
  _ <- pure layerList
  pure ()

-- Menu bar -----------------------------------------------------------------

buildMenuBar
  :: IORef AppState
  -> EditorService store
  -> IO Gtk.MenuBar
buildMenuBar _ _ = do
  menuBar <- new Gtk.MenuBar []

  -- File menu
  fileMenu  <- new Gtk.Menu []
  fileItem  <- new Gtk.MenuItem [ #label := "_File", #useUnderline := True ]
  set fileItem [ #submenu := fileMenu ]

  quitItem  <- new Gtk.MenuItem [ #label := "_Quit", #useUnderline := True ]
  _ <- on quitItem #activate Gtk.mainQuit
  #append fileMenu quitItem

  -- Help menu
  helpMenu  <- new Gtk.Menu []
  helpItem  <- new Gtk.MenuItem [ #label := "_Help", #useUnderline := True ]
  set helpItem [ #submenu := helpMenu ]

  aboutItem <- new Gtk.MenuItem [ #label := "_About", #useUnderline := True ]
  _ <- on aboutItem #activate $ do
         dlg <- new Gtk.MessageDialog
           [ #messageType := Gtk.MessageTypeInfo
           , #buttons     := Gtk.ButtonsTypeOk
           , #text        := "Syntic — watercolor editor"
           , #secondaryText :=
               T.concat
                 [ "A pure Haskell watercolor drawing engine."
                 , "\n\nArchitecture: Domain -> Application -> Adapter."
                 , "\nGUI: GTK 3 via gi-gtk."
                 ]
           ]
         _ <- #run dlg
         #destroy dlg
  #append helpMenu aboutItem

  #append menuBar fileItem
  #append menuBar helpItem
  pure menuBar

-- Toolbar ------------------------------------------------------------------

buildToolbar
  :: IORef AppState
  -> EditorService store
  -> IO (Gtk.Box, Gtk.RadioButton, Gtk.RadioButton, Gtk.RadioButton)
buildToolbar stateRef editor = do
  bar <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing     := 4
    , #marginTop    := 2
    , #marginBottom := 2
    , #marginStart  := 4
    , #marginEnd    := 4
    ]

  -- Tool selection (mutually-exclusive radio toggle buttons)
  brushBtn <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) "Brush"
  set brushBtn [ #drawIndicator := False, #active := True ]
  rectBtn  <- Gtk.radioButtonNewWithLabelFromWidget (Just brushBtn) "Rectangle"
  set rectBtn [ #drawIndicator := False ]
  ellBtn   <- Gtk.radioButtonNewWithLabelFromWidget (Just brushBtn) "Ellipse"
  set ellBtn [ #drawIndicator := False ]

  _ <- on brushBtn #toggled $ whenActive brushBtn $
         modifyIORef' stateRef (\s -> s { asTool = BrushTool })
  _ <- on rectBtn  #toggled $ whenActive rectBtn $
         modifyIORef' stateRef (\s -> s { asTool = RectangleTool })
  _ <- on ellBtn   #toggled $ whenActive ellBtn $
         modifyIORef' stateRef (\s -> s { asTool = EllipseTool })

  #packStart bar brushBtn False False 0
  #packStart bar rectBtn  False False 0
  #packStart bar ellBtn   False False 0

  sep1 <- new Gtk.Separator [ #orientation := Gtk.OrientationVertical ]
  #packStart bar sep1 False False 4

  -- Brush preset selection
  defBrushBtn <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) "Default"
  set defBrushBtn [ #drawIndicator := False, #active := True ]
  washBtn  <- Gtk.radioButtonNewWithLabelFromWidget (Just defBrushBtn) "Wash"
  set washBtn [ #drawIndicator := False ]
  detBtn   <- Gtk.radioButtonNewWithLabelFromWidget (Just defBrushBtn) "Detail"
  set detBtn [ #drawIndicator := False ]

  _ <- on defBrushBtn #toggled $ whenActive defBrushBtn $
         modifyIORef' stateRef (\s -> s { asBrushPreset = DefaultBrush })
  _ <- on washBtn     #toggled $ whenActive washBtn $
         modifyIORef' stateRef (\s -> s { asBrushPreset = WashBrush })
  _ <- on detBtn      #toggled $ whenActive detBtn $
         modifyIORef' stateRef (\s -> s { asBrushPreset = DetailBrush })

  #packStart bar defBrushBtn False False 0
  #packStart bar washBtn     False False 0
  #packStart bar detBtn      False False 0

  sep2 <- new Gtk.Separator [ #orientation := Gtk.OrientationVertical ]
  #packStart bar sep2 False False 4

  -- Undo / Redo
  undoBtn <- new Gtk.Button [ #label := "Undo" ]
  redoBtn <- new Gtk.Button [ #label := "Redo" ]

  _ <- on undoBtn #clicked $ do
         st <- readIORef stateRef
         case undoDocument editor (asStore st) (asDocumentId st) of
           Right (store', _) ->
             writeIORef stateRef (st { asStore = store' })
           Left _ -> pure ()
  _ <- on redoBtn #clicked $ do
         st <- readIORef stateRef
         case redoDocument editor (asStore st) (asDocumentId st) of
           Right (store', _) ->
             writeIORef stateRef (st { asStore = store' })
           Left _ -> pure ()

  #packStart bar undoBtn False False 0
  #packStart bar redoBtn False False 0

  pure (bar, brushBtn, rectBtn, ellBtn)

whenActive :: Gtk.RadioButton -> IO () -> IO ()
whenActive btn action = do
  active <- get btn #active
  when active action

-- Side panel (layer list + controls) --------------------------------------

buildSidePanel
  :: IORef AppState
  -> EditorService store
  -> Gtk.DrawingArea
  -> IO (Gtk.Box, Gtk.ListBox, IO ())
buildSidePanel stateRef editor canvas = do
  side <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing     := 4
    , #marginTop    := 4
    , #marginBottom := 4
    , #marginStart  := 4
    , #marginEnd    := 4
    ]
  #setSizeRequest side 220 (-1)

  header <- new Gtk.Label [ #label := "Layers", #xalign := 0 ]
  #packStart side header False False 0

  listFrame <- new Gtk.Frame [ #shadowType := Gtk.ShadowTypeIn ]
  listScroll <- new Gtk.ScrolledWindow []
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle ]
  #add listScroll listBox
  #add listFrame listScroll
  #packStart side listFrame True True 0

  -- Row-selected -> update active layer
  _ <- on listBox #rowSelected $ \mbRow -> case mbRow of
    Nothing  -> pure ()
    Just row -> do
      name <- #getName row
      let nameStr = T.unpack name
      when (not (null nameStr)) $
        modifyIORef' stateRef (\s -> s { asActiveLayer = LayerId nameStr })

  -- Add/remove layer buttons
  layerBtnBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing     := 4
    ]
  addLayerBtn <- new Gtk.Button [ #label := "Add" ]
  delLayerBtn <- new Gtk.Button [ #label := "Remove" ]
  #packStart layerBtnBox addLayerBtn True True 0
  #packStart layerBtnBox delLayerBtn True True 0
  #packStart side layerBtnBox False False 0

  -- Mutable counter for new-layer ids
  layerCounter <- newIORef (0 :: Int)

  -- Populate the ListBox from the current document state.
  let rebuild :: IO ()
      rebuild = do
        -- remove existing rows
        children <- #getChildren listBox
        mapM_ (#remove listBox) children

        st <- readIORef stateRef
        case getDocument editor (asStore st) (asDocumentId st) of
          Left _    -> pure ()
          Right doc -> do
            let layers = documentLayers doc
            forM_ layers $ \lyr -> do
              row <- new Gtk.ListBoxRow []
              let lid  = layerId lyr
                  name = T.pack (layerName lyr)
                  tag  = T.pack (unLayerId lid)
              #setName row tag
              lbl <- new Gtk.Label
                [ #label  := name
                , #xalign := 0
                , #marginStart := 6
                , #marginEnd   := 6
                ]
              #add row lbl
              #add listBox row
              when (lid == asActiveLayer st) (#selectRow listBox (Just row))
            #showAll listBox

  _ <- on addLayerBtn #clicked $ do
         n <- atomicModifyIORef' layerCounter (\c -> (c + 1, c + 1))
         let lid = LayerId ("layer-" <> show n)
             nm  = "Layer " <> show n
         st <- readIORef stateRef
         case applyDocumentCommand editor (asStore st) (asDocumentId st)
                (AddLayer lid nm) of
           Right (store', _) -> do
             writeIORef stateRef (st { asStore = store', asActiveLayer = lid })
             rebuild
             #queueDraw canvas
           Left _ -> pure ()

  _ <- on delLayerBtn #clicked $ do
         st <- readIORef stateRef
         let active = asActiveLayer st
         when (active /= defaultLayerId) $
           case applyDocumentCommand editor (asStore st) (asDocumentId st)
                  (RemoveLayer active) of
             Right (store', _) -> do
               writeIORef stateRef
                 (st { asStore = store', asActiveLayer = defaultLayerId })
               rebuild
               #queueDraw canvas
             Left _ -> pure ()

  pure (side, listBox, rebuild)

-- Input handling -----------------------------------------------------------

handlePress :: IORef AppState -> Int -> Int -> IO ()
handlePress stateRef x y = do
  st <- readIORef stateRef
  let p = Point x y
  case asTool st of
    BrushTool ->
      writeIORef stateRef
        (st { asDrag = BrushStroking [StrokeSample p 0.7] })
    RectangleTool ->
      writeIORef stateRef (st { asDrag = DraggingRect p p })
    EllipseTool ->
      writeIORef stateRef (st { asDrag = DraggingEllipse p p })

-- | Update drag state for a motion event. Returns True if the canvas
-- should be redrawn.
handleMotion :: IORef AppState -> Int -> Int -> IO Bool
handleMotion stateRef x y = do
  st <- readIORef stateRef
  case asDrag st of
    Idle -> pure False
    BrushStroking samples -> do
      let newSample = StrokeSample (Point x y) 0.7
      writeIORef stateRef (st { asDrag = BrushStroking (newSample : samples) })
      pure True
    DraggingRect start _ -> do
      writeIORef stateRef (st { asDrag = DraggingRect start (Point x y) })
      pure True
    DraggingEllipse start _ -> do
      writeIORef stateRef (st { asDrag = DraggingEllipse start (Point x y) })
      pure True

-- | Commit the current drag as a shape via the editor.
handleRelease
  :: IORef AppState
  -> EditorService store
  -> Int
  -> Int
  -> IO (Either ApplicationError String)
handleRelease stateRef editor x y = do
  st <- readIORef stateRef
  case asDrag st of
    Idle -> pure (Right (asStatus st))
    BrushStroking samples -> do
      let finalSample = StrokeSample (Point x y) 0.7
          allSamples  = reverse (finalSample : samples)
      case NE.nonEmpty allSamples of
        Nothing -> do
          writeIORef stateRef (st { asDrag = Idle })
          pure (Right "Stroke discarded (no samples)")
        Just ne -> do
          let brush = presetBrush (asBrushPreset st)
              bs    = BrushStroke
                        { strokeBrush   = brush
                        , strokeColor   = asColor st
                        , strokeSamples = ne
                        }
              (st', sid) = freshShapeId st
              shape = Shape
                        { shapeId       = sid
                        , shapeGeometry = BrushStrokeGeometry bs
                        , shapeStyle    = Style { fillColor = Nothing, stroke = Nothing }
                        }
          applyAddShape stateRef editor st' shape "Brush stroke added"
    DraggingRect (Point x0 y0) _ -> do
      let (ox, oy, w, h) = normBox x0 y0 x y
      if w <= 0 || h <= 0
        then do
          writeIORef stateRef (st { asDrag = Idle })
          pure (Right "Rectangle discarded (zero size)")
        else do
          let rect = Rectangle
                       { rectangleOrigin = Point ox oy
                       , rectangleWidth  = fromIntegral w
                       , rectangleHeight = fromIntegral h
                       }
              (st', sid) = freshShapeId st
              shape = Shape
                        { shapeId       = sid
                        , shapeGeometry = RectangleGeometry rect
                        , shapeStyle    = Style
                                            { fillColor = Just (asColor st)
                                            , stroke    = Nothing
                                            }
                        }
          applyAddShape stateRef editor st' shape "Rectangle added"
    DraggingEllipse (Point x0 y0) _ -> do
      let (ox, oy, w, h) = normBox x0 y0 x y
      if w <= 0 || h <= 0
        then do
          writeIORef stateRef (st { asDrag = Idle })
          pure (Right "Ellipse discarded (zero size)")
        else do
          let rx  = max 1 (w `div` 2)
              ry  = max 1 (h `div` 2)
              cx  = ox + rx
              cy  = oy + ry
              ell = Ellipse
                      { ellipseCenter  = Point cx cy
                      , ellipseRadiusX = fromIntegral rx
                      , ellipseRadiusY = fromIntegral ry
                      }
              (st', sid) = freshShapeId st
              shape = Shape
                        { shapeId       = sid
                        , shapeGeometry = EllipseGeometry ell
                        , shapeStyle    = Style
                                            { fillColor = Just (asColor st)
                                            , stroke    = Nothing
                                            }
                        }
          applyAddShape stateRef editor st' shape "Ellipse added"

applyAddShape
  :: IORef AppState
  -> EditorService store
  -> AppState
  -> Shape
  -> String
  -> IO (Either ApplicationError String)
applyAddShape stateRef editor st shape successMsg = do
  let cmd = AddShape (asActiveLayer st) shape
  case applyDocumentCommand editor (asStore st) (asDocumentId st) cmd of
    Right (store', _) -> do
      writeIORef stateRef
        (st { asStore = store', asDrag = Idle })
      pure (Right successMsg)
    Left err -> do
      writeIORef stateRef (st { asDrag = Idle })
      pure (Left err)

normBox :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
normBox x1 y1 x2 y2 =
  ( min x1 x2
  , min y1 y2
  , abs (x2 - x1)
  , abs (y2 - y1)
  )

-- | Fallback empty document for the very rare case where the store has
-- lost the document mid-render (defensive; keeps the handler total).
emptyDoc :: CanvasSize -> Document
emptyDoc cs =
  emptyDocument (DocumentId "fallback") cs (opaque 255 255 255)
