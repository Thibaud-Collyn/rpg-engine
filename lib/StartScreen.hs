module StartScreen where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import GHC.IO (unsafePerformIO)
import Datastructures
import LevelLoader
import GameLogica
import TextureLoader
import GameRenderer

--------------------- Handle start screen ---------------------
handleLevelSelectInput :: Event -> Game -> Game
handleLevelSelectInput ev game | isKey KeyEnter ev = unsafePerformIO (initGame (selector game) (gameLevels !! selector game))
                               | isKey KeyUp ev = moveSelector (-1) game
                               | isKey KeyDown ev = moveSelector 1 game
                               | otherwise = game

moveSelector :: Int -> Game -> Game
moveSelector x game | canMoveSelector x game = game { selector = selector game + x }
                    | otherwise = game

canMoveSelector :: Int -> Game -> Bool
canMoveSelector x game | selector game + x < 0 = False
                       | selector game + x > length gameLevels - 1 = False
                       | otherwise = True

--------------------- Render start screen ---------------------
renderStartScreen :: Game -> Picture
renderStartScreen game = pictures [renderBackground, renderStartText, renderLevelKeys game]

renderStartText :: Picture
renderStartText = translate (-280.0) 150.0 (scale 0.3 0.3 (text "Press enter to select level"))

renderLevelKeys :: Game -> Picture
renderLevelKeys game = pictures [if lvl == (selector game) then renderLevelKey lvl True else renderLevelKey lvl False | lvl <- [0..(length gameLevels) - 1]]

renderLevelKey :: Int -> Bool -> Picture
renderLevelKey lvl isSelected | isSelected = translate (-100.0) (fromIntegral (80 - (lvl*50))) (scale 0.3 0.3 (color white (text ("Level " ++ (show (lvl+1))))))
                              | otherwise = translate (-100.0) (fromIntegral (80 - (lvl*50))) (scale 0.3 0.3 (text ("Level " ++ (show (lvl+1)))))

--------------------- Handle end screen ---------------------
handleEndInput :: Event -> Game -> Game
handleEndInput ev game | isKey KeyEnter ev = emptyGame
                       | otherwise = game

--------------------- Render end screen ---------------------
renderEndScreen :: Picture
renderEndScreen = pictures [renderBackground, renderEndText]

renderEndText :: Picture
renderEndText = pictures [translate (-130.0) 50.0 (scale 0.4 0.4 (text "You Won!")), translate (-280.0) 0.0 (scale 0.2 0.2 (text "Press enter to return to the level selector"))]