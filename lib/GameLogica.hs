module GameLogica where

import Datastructures
import LevelLoader
import Graphics.Gloss.Interface.IO.Game
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import GHC.IO (unsafePerformIO)
import System.Directory (getDirectoryContents)
import Data.List
import Data.Function
import Functions

-- Used to load in all the level file names in alphabetical order
gameLevels :: [String]
gameLevels = sort findLevelFiles

findLevelFiles :: [String]
{-# NOINLINE findLevelFiles #-}
findLevelFiles = unsafePerformIO $ do
      levels <- getDirectoryContents "levels"
      return $ map (reverse . drop 4 . reverse) $ filter (\x -> getExtension x == "txt") levels

getExtension :: String -> String
getExtension [] = []
getExtension (x:xs)
    | x == '.' = xs
    | otherwise = getExtension xs

--------------------- Input Handeling ---------------------
handleLevelInput :: Event -> Game -> Game
handleLevelInput ev game | isKey KeyUp ev = movePlayer (0,1) game
                         | isKey KeyDown ev = movePlayer (0,-1) game
                         | isKey KeyLeft ev = movePlayer (-1,0) game
                         | isKey KeyRight ev = movePlayer (1,0) game
                         | isNumber ev && read (getNumber ev) < length (getActions game) = applyFunction game (action (getActions game !!(read (getNumber ev) :: Int)))
                         | otherwise = game

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isKey _  _                                   = False

-- detect if key pressed is a normal letter
isNormalKey :: Char -> Event -> Bool
isNormalKey k1 (EventKey (Char k2) Graphics.Gloss.Interface.IO.Game.Down _ _) = k1 == k2
isNormalKey _ _ = False

isNumber :: Event -> Bool
isNumber (EventKey (Char k) Graphics.Gloss.Interface.IO.Game.Down _ _) = k `elem` ['0'..'9']
isNumber _ = False

getNumber :: Event -> String
getNumber (EventKey (Char k) Graphics.Gloss.Interface.IO.Game.Down _ _ ) = [k]

--------------------- Player movement ---------------------
movePlayer :: (Int, Int) -> Game -> Game
movePlayer (x, y) game | canMoveTo (newX, newY) game = game {player = (player game) {playerX = newX, playerY = newY}}
                       | otherwise = game
                          where newX = playerX (player game) + x
                                newY = playerY (player game) + y

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

--------------------- gameState checks ---------------------
gameState :: Game -> Game
gameState game | (state game) == Selecting = game
               | playerHP p <= 0 = unsafePerformIO (initGame (selector game) (gameLevels !! selector game))
               | (getTileAt pPos currentLvlLayout) == End = nextLevelOrEnd game
               | otherwise = game
               where p = player game
                     pPos = (playerX p, playerY p)
                     currentLvlLayout = layout (levels game !! currentLevel game)

nextLevelOrEnd :: Game -> Game
nextLevelOrEnd game | currentLevel game == length (levels game) - 1 = game {state = Completed}
                    | otherwise = setPlayerLocation (game {currentLevel = currentLevel game + 1})

enemyActions :: Game -> Game
enemyActions game = enemyAttack (despawnEnemies game)

despawnEnemies :: Game -> Game
despawnEnemies game = game {levels = replaceNth (currentLevel game) (despawnEnemiesInLevel (levels game !! currentLevel game)) (levels game)}

despawnEnemiesInLevel :: Level -> Level
despawnEnemiesInLevel level = level {entities = [e | e <- entities level, isNothing (entityHP e) || fromJust (entityHP e) > 0]}

enemyAttack :: Game -> Game
enemyAttack game = game {player = (player game) {playerHP = decreasePlayerHP (levels game !! currentLevel game) (player game) (ticks game)}, ticks = (ticks game) + 1}

decreasePlayerHP :: Level -> Player -> Int -> Int
decreasePlayerHP level player ticks = (playerHP player) - sum [fromMaybe 0 (entityValue e) | e <- (entities level), isEntityInRange player e && (ticks `mod` 60) == 0]

--------------------- Player interaction ---------------------
getActions :: Game -> [Action]
getActions game = [action | action <- getAllActions game, canDoAction (conditions action) game]

getAllActions :: Game -> [Action]
getAllActions game = concat [entityActions e | e <- senseEntities game] ++ concat [itemActions i | i <- senseItems game]

senseEntities :: Game -> [Entity]
senseEntities game = [e | e <- entityList, inRange playerLoc (entityX e, entityY e)] where playerLoc = (playerX (player game), playerY (player game))
                                                                                           entityList = entities (levels game !! currentLevel game)


senseItems :: Game -> [GameItem]
senseItems game = [i | i <- itemList, inRange playerLoc (itemX i, itemY i)] where playerLoc = (playerX (player game), playerY (player game))
                                                                                  itemList = items (levels game !! currentLevel game)

applyFunction :: Game -> Function -> Game
applyFunction game func | Datastructures.id func == "retrieveItem" = retrieveItem game (argumentToId (head (args func)))
                        | Datastructures.id func == "useItem" = useItem game (argumentToId (head (args func)))
                        | Datastructures.id func == "increasePlayerHp" = increasePlayerHP game (argumentToId (head (args func)))
                        | Datastructures.id func == "decreaseHp" = decreaseHP game (argumentToId (head (args func))) (argumentToId ((args func)!!1))
                        | Datastructures.id func == "leave" = leave game
                        | otherwise = game

canDoAction :: [Function] -> Game -> Bool
canDoAction [] game = True
canDoAction conditions game = conditionMet game (head conditions) && canDoAction (tail conditions) game

conditionMet :: Game -> Function -> Bool
conditionMet game func | Datastructures.id func == "not" = not (conditionMet game (argumentToFunction (head (args func))))
                       | Datastructures.id func == "inventoryFull" = inventoryFull (player game)
                       | Datastructures.id func == "inventoryContains" = inventoryContains (player game) (argumentToId (head (args func)))
                       | otherwise = False
