{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module MyWidgets.Notification (notification) where

import Data.GI.Base
import Data.Text (pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified GI.Astal as Astal
import qualified GI.AstalApps as Apps
import qualified GI.AstalBattery as Battery
import qualified GI.AstalBluetooth as Bluetooth
import qualified GI.AstalHyprland as Hyprland
import qualified GI.AstalIO as AstalIO
import qualified GI.AstalMpris as Mpris
import qualified GI.AstalNetwork as Network
import qualified GI.AstalNotifd as Notifd
import qualified GI.AstalPowerProfiles as PowerProfiles
import qualified GI.AstalWp as Wp
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk


notification :: Int -> IO Astal.Window
notification monitor = do
  window <-
    new
      Astal.Window
      [ #monitor := fromIntegral monitor,
        #anchor := [Astal.WindowAnchorTop, Astal.WindowAnchorRight],
        #exclusivity := Astal.ExclusivityExclusive,
        #marginTop := 0,
        #marginBottom := 0,
        #child :=> new Astal.Box []
      ]
  window.showAll
  return window
