{-# language OverloadedStrings #-}

module MyLib where

import DearImGui qualified

myGUI :: IO ()
myGUI = do
  DearImGui.text "Such Fast reload!"
  -- DearImGui.plotLines "sin(x)" [sin x | x <- [0..42]]
  -- DearImGui.plotLines "cos(x)" [cos x | x <- [0..42]]
  -- DearImGui.plotLines "plot" [cos x + sin (x * 2) | x <- [0..42]]
