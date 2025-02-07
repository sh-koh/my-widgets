{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module MyWidgets.Main (mywidgets) where

import MyWidgets.Bar
import MyWidgets.Notification
import MyWidgets.Panel
import Control.Monad (void, (<=<))
import Data.GI.Base
import qualified GI.Astal as Astal


activate :: Astal.Application -> IO ()
activate app = do
  Astal.applicationGetMonitors app >>= \mons -> do
    let numMonitors = length mons
    mapM_ (app.addWindow <=< bar) [0 .. fromIntegral (numMonitors - 1)]

-- sass :: String -> Maybe String -> IO ()
-- sass input output = callCommand . unwords $ case output of
--   Nothing -> ["sass", "-q", input]
--   Just out -> ["sass", "-q", "--no-source-map", input, out]

mywidgets :: IO ()
mywidgets = do
  app <- new Astal.Application [On #activate (activate ?self)]
  void $ app.run Nothing
