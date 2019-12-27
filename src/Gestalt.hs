{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Gestalt
    ( initializeGestalt,
      cleanupGestalt,
      newTickEvent,
      newSDLEvent,
      keyPressed,
      renderFrame
    ) where

import Control.Monad.IO.Class
import System.Clock
import qualified SDL
import Linear (V2(..), V3(..), V4(..))
import Control.Wire
import Control.Wire.Controller
import Graphics.Rendering.OpenGL
import Prelude hiding ((.), id)

gestaltWindowConfig :: SDL.WindowConfig
gestaltWindowConfig = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.Windowed
  , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = False
  , SDL.windowInitialSize     = V2 800 600
  , SDL.windowVisible         = True
  }

initializeGestalt :: IO SDL.Window
initializeGestalt = do
  SDL.initializeAll
  window <- SDL.createWindow "My Gestalt Application" gestaltWindowConfig
  SDL.glCreateContext window
  return window

cleanupGestalt :: SDL.Window -> IO ()
cleanupGestalt window = do
  SDL.destroyWindow window

newSDLEvent :: (MonadIO m) => Wire m a (Event SDL.EventPayload)
newSDLEvent = proc _ ->
    newEvent -< liftIO $ do
        possible_event <- SDL.pollEvent
        case possible_event of
          Just e -> return . Just $ SDL.eventPayload e
          Nothing -> return Nothing

newTickEvent :: (MonadIO m) => Wire m a (Event Double)
newTickEvent = proc _ -> do
    times <- newEvent -< Just <$> getT
    withM_ unfoldE getT -< fmap (\t t' -> (secs (t - t'), t)) times

    where
    secs = (/ 1000000000) . fromInteger . toNanoSecs
    getT = liftIO (getTime Monotonic)

keyPressed :: SDL.EventPayload -> Maybe SDL.Keycode
keyPressed (SDL.KeyboardEvent e) = if SDL.keyboardEventKeyMotion e == SDL.Pressed then Just . SDL.keysymKeycode $ SDL.keyboardEventKeysym e else Nothing
keyPressed _ = Nothing

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

renderFrame :: SDL.Window -> IO () 
renderFrame window = do
  clear [ColorBuffer]
  renderPrimitive TriangleFan $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
  SDL.glSwapWindow window

