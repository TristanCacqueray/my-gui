-- NOTE: If this is file is edited, please also copy and paste it into
-- README.md.

{-# language OverloadedStrings, LambdaCase, BlockArguments #-}

module Main where

import MyLib

import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL

import Graphics.GL
import SDL

import Control.Monad.Managed
import Control.Monad.IO.Class ()
import Control.Monad (unless, void)
import Control.Exception (bracket, bracket_)

import Data.IORef
import Control.Concurrent
import Foreign.Store qualified as FS

-- Store the render function behind a IORef for hot reload
renderFuncStore :: FS.Store (IORef (Window -> IO ()))
renderFuncStore = FS.Store 0

-- The ghcid entry point
mainGHCID :: IO ()
mainGHCID = FS.lookupStore renderFuncStoreID >>= \case
  -- This is a fresh reload, initialize the store and fork the main thread
  Nothing -> do
    -- Create and store the render func IORef
    renderRef <- newIORef renderFunc
    FS.writeStore renderFuncStore renderRef

    -- Fork a OSThread that persists ghci reload
    void $ Control.Concurrent.forkOS do
      runMain \win -> do
        renderFun <- readIORef renderRef
        renderFun win
      -- If the app quit, cleanup the store to restart fresh
      FS.deleteStore renderFuncStore

  -- This is a hot reload, just update the stored render func IORef
  Just store -> do
    renderRef <- FS.readStore store
    writeIORef renderRef renderFunc
 where
   -- For some reason, FS.lookupStore needs the newtyped Store value
   FS.Store renderFuncStoreID = renderFuncStore

-- The regular entry point
main :: IO ()
main = runMain renderFunc

runMain :: (Window -> IO ()) -> IO ()
runMain theRenderFunc = do
  -- Initialize SDL
  initializeAll

  runManaged $ do
    -- Create a window using SDL; as we're using OpenGL, we enable OpenGL too
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ runLoop window
  where
  runLoop window = unlessQuit do
    theRenderFunc window
    runLoop window

  -- Process the event loop
  unlessQuit action = do
    shouldQuit <- gotQuitEvent
    unless shouldQuit action

  gotQuitEvent = do
    ev <- pollEventWithImGui

    case ev of
      Nothing ->
        return False
      Just event ->
        (isQuit event ||) <$> gotQuitEvent

  isQuit event =
    eventPayload event == QuitEvent

renderFunc :: Window -> IO ()
renderFunc window = do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    myGUI

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window
