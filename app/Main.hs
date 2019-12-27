{-# LANGUAGE Arrows #-}
module Main where

import Prelude hiding ((.),id)

import Control.Monad.IO.Class
import Control.Wire
import Control.Wire.Controller
import Gestalt

import Control.Arrow
import Data.Align
import SDL (Window(..))
import qualified SDL.Event as SDLE
import SDL.Input.Keyboard.Codes

main :: IO ()
main = do
  window <- initializeGestalt
  control $ proc _ -> do myApp -< window
  cleanupGestalt window

myApp :: (MonadIO m) => Wire m SDL.Window (Event ())
myApp = proc window -> do
--    deltas <- newTickEvent -< ()
--    fps <- hold 0 . (fmap recip <$> average 25) -< deltas
    event <- newSDLEvent -< ()
    keys <- id -< catMapE keyPressed event

    animate -< liftIO $ do
      renderFrame window

    id -< () <$  (filterE (== KeycodeQ) keys) `align` (filterE (== KeycodeEscape) keys) `align` (filterE (== SDLE.QuitEvent) event)
