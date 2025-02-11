{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module MyWidgets.Bar (bar) where

import Control.Monad (forM_, void, when, (<=<))
import Data.GI.Base
import Data.GI.Base.GValue (fromGValue, toGValue)
import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified GI.Astal as Astal
import qualified GI.AstalBattery as Battery
import qualified GI.AstalBluetooth as Bluetooth
import qualified GI.AstalHyprland as Hyprland
import qualified GI.AstalIO as AIO
import qualified GI.AstalNetwork as Network
import qualified GI.AstalWp as Wp
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

workspaces :: Int -> IO [Astal.Button]
workspaces monitor = do
  hyprland <- Hyprland.getDefault
  fw <- Hyprland.hyprlandGetFocusedWorkspace hyprland
  cs <- Hyprland.hyprlandGetClients hyprland
  state <-
    new
      AIO.Variable
      [ #value :=> get hyprland #focusedWorkspace >>= \x -> get x #id >>= toGValue
      ]
  mapM
    ( \btnNum -> do
        let wsId = btnNum + monitor * 10
        new
          Astal.Button
          [ #valign := Gtk.AlignCenter,
            #halign := Gtk.AlignCenter,
            #child :=>
              new
                Astal.Label
                [ #label := pack $ show btnNum
                ],
            On #released $
              void $
                Hyprland.hyprlandMessage hyprland $
                  pack $
                    "dispatch workspace " <> show wsId
          ]
    )
    [1 .. 9]

time :: IO Astal.Label
time = do
  clock <- new Astal.Label []
  void $
    AIO.timeInterval 1000 Nothing >>= \interval -> on interval #now $ do
      clock.setLabel . pack . formatTime defaultTimeLocale "%H:%M" =<< getZonedTime
  return clock

left :: IO Astal.Box
left = do
  new
    Astal.Box
    [ #valign := Gtk.AlignCenter,
      #halign := Gtk.AlignStart
    ]

center :: Int -> IO Astal.Box
center monitor = do
  btns <- workspaces monitor
  box <-
    new
      Astal.Box
      [ #halign := Gtk.AlignCenter,
        #valign := Gtk.AlignCenter
      ]
  void $ Astal.boxSetChildren box btns
  return box

right :: IO Astal.Box
right = do
  new
    Astal.Box
    [ #valign := Gtk.AlignCenter,
      #halign := Gtk.AlignEnd,
      #child :=> time
    ]

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
          new
            Astal.CenterBox
            [ #startWidget :=> left,
              #centerWidget :=> center monitor,
              #endWidget :=> right
            ]
      ]
  window.showAll
  return window
