{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module MyWidgets.Bar (bar) where

import Control.Monad (forM_, void, when, (<=<))
import Data.GI.Base
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
  -- On utilise wss cette fois
  fw <- Hyprland.hyprlandGetFocusedWorkspace hyprland
  cs <- Hyprland.hyprlandGetClients hyprland
  wss <- Hyprland.hyprlandGetWorkspaces hyprland

  -- On crée une variable pour stocker l'état
  state <-
    new
      AIO.Variable
      [ #value :=> Astal.toGValue ([] :: [(Int, Bool, Bool)])
      ]

  -- Fonction pour mettre à jour l'état
  let updateWorkspaces = do
        focusedWs <- Hyprland.hyprlandGetFocusedWorkspace hyprland
        focusedId <- Hyprland.getWorkspaceId focusedWs

        -- On traite chaque workspace
        workspaceStates <-
          mapM
            ( \ws -> do
                wsId <- Hyprland.getWorkspaceId ws
                let isOccupied = any (\c -> Hyprland.getClientWorkspace c == fromIntegral wsId) cs
                let isFocused = wsId == focusedId
                return (wsId, isOccupied, isFocused)
            )
            wss

        set state [#value :=> Astal.toGValue workspaceStates]

  -- On crée les boutons pour les workspaces 1-9
  buttons <-
    mapM
      ( \btnNum -> do
          let wsId = fromIntegral $ btnNum + monitor * 10
          btn <-
            new
              Astal.Button
              [ #valign := Gtk.AlignCenter,
                #halign := Gtk.AlignCenter,
                #child :=> new Astal.Label [#label := pack $ show btnNum],
                On #released $
                  void $
                    Hyprland.hyprlandMessage hyprland $
                      pack $
                        "dispatch workspace " ++ show wsId
              ]

          -- On observe les changements d'état
          void $ on state #notify $ \_ -> do
            stateValue <- AIO.getVariableValue state
            forM_ stateValue $ \(id', isOccupied, isFocused) ->
              when (id' == wsId) $ do
                let style = case (isOccupied, isFocused) of
                      (True, True) -> "focused-occupied"
                      (True, False) -> "occupied"
                      (False, True) -> "focused-empty"
                      (False, False) -> "empty"
                Astal.widgetToggleClassName btn (pack style) True
          return btn
      )
      [1 .. 9]

  -- Premier update
  updateWorkspaces

  -- On observe les changements de workspace et clients
  void $ on hyprland #workspaceChanged $ const updateWorkspaces
  void $ on hyprland #clientChanged $ const updateWorkspaces

  return buttons

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
