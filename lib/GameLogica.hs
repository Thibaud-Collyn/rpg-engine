module GameLogica where

import Datastructures
import LevelLoader
import Graphics.Gloss.Interface.IO.Game
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing)

handleInput :: Event -> Game -> Game
handleInput ev game | isKey KeyUp ev = movePlayer (0,1) game
                    | isKey KeyDown ev = movePlayer (0,-1) game
                    | isKey KeyLeft ev = movePlayer (-1,0) game
                    | isKey KeyRight ev = movePlayer (1,0) game
                    | otherwise = game

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isKey _  _                                   = False

-- detect if key pressed is a normal letter
isNormalKey :: Char -> Event -> Bool
isNormalKey k1 (EventKey (Char k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isNormalKey _ _ = False

movePlayer :: (Int, Int) -> Game -> Game
movePlayer (x, y) game | canMoveTo (newX, newY) game = game {player = (player game) {playerX = newX, playerY = newY}}
                       | otherwise = game
                          where newX = playerX (player game) + x
                                newY = playerY (player game) + y

--FIXME: check for unopened door entity
canMoveTo :: (Int, Int) -> Game -> Bool
canMoveTo (x, y) game | getTileAt (x, y) (layout (levels game !! currentLevel game)) == Wall = False
                      | hasClosedDoor (x, y) (levels game !! currentLevel game) = False
                      | otherwise = True

getTileAt :: (Int, Int) -> [TileLine] -> Tile
getTileAt (x, y) layout = (tileLineToArray (layout !! y)) !! x

hasClosedDoor :: (Int, Int) -> Level -> Bool
hasClosedDoor (x, y) level | length door == 0 = False
                           | entityX (head door) == (x-1) && entityY (head door) == (y-1) && isNothing (entityValue (head door)) = True
                           | otherwise = False
                            where door = getEntity level "door"

getEntity :: Level -> String -> [Entity]
getEntity level id = [entity | entity <- entities level, entityId entity == id]

entityAt :: (Int, Int) -> [Entity] -> Maybe Entity
entityAt (x, y) entities = undefined
