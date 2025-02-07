{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module MyWidgets.Bar (bar) where

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


workspaces :: Int -> IO [Astal.Button]
workspaces monitor = do
  [ b | b <- new Astal.Button
    [ #cursor := "pointer",
      #valign := Gtk.AlignCenter,
      #halign := Gtk.AlignCenter,
      #child :=> new Astal.Label [ #label := monitor ]
    ] * 9
  ]

left :: IO Astal.Box
left = do
  new Astal.Box []

center :: IO Astal.Box
center = do
  clock <- new Astal.Label []
  _ <-
    AstalIO.timeInterval 1000 Nothing >>= \interval -> on interval #now $ do
      clock.setLabel . pack . formatTime defaultTimeLocale "%I:%M" =<< getZonedTime

  new Astal.Box [#child := clock]

right :: IO Astal.Box
right = do
  new Astal.Box []

bar :: Int -> IO Astal.Window
bar monitor = do
  window <-
    new
      Astal.Window
      [ #monitor := fromIntegral monitor,
        #anchor := [Astal.WindowAnchorTop, Astal.WindowAnchorLeft, Astal.WindowAnchorRight],
        #exclusivity := Astal.ExclusivityExclusive,
        #marginTop := 0,
        #marginBottom := 0,
        #child :=>
          new Astal.CenterBox
            [ #startWidget :=> left,
              #centerWidget :=> center,
              #endWidget :=> right
            ]
      ]
  window.showAll
  return window
