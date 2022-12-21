module LevelLoader where

import Datastructures
import Parser
import Text.Parsec
import Data.Maybe (isNothing)

initGame :: String -> IO Game
initGame levelName = do
    levelFile <- readFile ("levels/" ++ levelName ++ ".txt")
    let levelJson = parse parseJSON "Could not parse file" levelFile
    case levelJson of
        Prelude.Left err -> error $ show err
        Prelude.Right levelJson -> return $ initGameFromJson levelJson

initGameFromJson :: JSON -> Game
initGameFromJson (Object pairs) = setPlayerLocation (Game (initPlayer (getValue (getPair pairs (ID "player")))) (initLevels (getValue ((getPair pairs (ID "levels"))))) 0)

initPlayer :: JSON -> Player
initPlayer (Object pairs) = Player (toInt (getValue (getPair pairs (ID "hp")))) (initItems (getValue (getPair pairs (ID "inventory")))) 0 0 Datastructures.Right

setPlayerLocation :: Game -> Game
setPlayerLocation game = game{player = (player game){playerX = x, playerY = y}} where (x, y) = getStartLocation ((levels game) !! (currentLevel game)) 

getStartLocation :: Level -> (Int, Int)
getStartLocation level = head [(x, y) |y <- [0..length tileLines-1], x <- [0..(length (tileLineToArray (tileLines!!y))) -1], tileLineToArray((tileLines!!y))!!x == Start] where tileLines = (layout level)

------------------ Initializing levels ------------------
initLevels :: JSON -> [Level]
initLevels (Array lvl) = map initLevel lvl

initLevel :: JSON -> Level
initLevel (Object lvl) = Level (initLayout (getValue (getPair lvl (ID "layout")))) (initItems (getValue (getPair lvl (ID "items")))) (initEntities (getValue (getPair lvl (ID "entities"))))

initLayout :: JSON -> [TileLine]
initLayout (Layout []) = []
initLayout tileLines = reverse (layoutToArray tileLines)

------------------ Initializing items ------------------
initItems :: JSON -> [GameItem]
initItems (Array []) = []
initItems (Array items) = map initItem items

initItem :: JSON -> GameItem
initItem (Object item) = GameItem (toStr (getValue (getPair item (ID "id")))) (toInt (getValue (getPair item (ID "x")))) (toInt (getValue (getPair item (ID "y")))) (toStr (getValue (getPair item (ID "name")))) 
    (toStr (getValue (getPair item (ID "description")))) (toUseTimes (getValue (getPair item (ID "useTimes")))) (initActions (getValue (getPair item (ID "actions"))))

------------------ Initializing entities ------------------
initEntities :: JSON -> [Entity]
initEntities (Array []) = []
initEntities (Array entities) = map initEntity entities

initEntity :: JSON -> Entity
initEntity (Object entity) = Entity (toStr (getValue (getPair entity (ID "id")))) (toInt (getValue (getPair entity (ID "x")))) (toInt (getValue (getPair entity (ID "y")))) (toStr (getValue (getPair entity (ID "name")))) 
    (toStr (getValue (getPair entity (ID "description")))) (initDirection (Object entity)) (initHP (Object entity)) (initValue (Object entity)) (initActions (getValue (getPair entity (ID "actions"))))

-- These functions are used to initialize direction, hp and value of an entity becausd they are optional
initDirection :: JSON -> Maybe Direction
initDirection (Object entity) | isNothing (getMaybePair entity (ID "direction")) = Nothing
                              | otherwise = Just (toDirection (getValue (getPair entity (ID "direction"))))

initHP :: JSON -> Maybe Int
initHP (Object entity) | isNothing (getMaybePair entity (ID "hp")) = Nothing
                       | otherwise = Just (toInt (getValue (getPair entity (ID "hp"))))

initValue :: JSON -> Maybe Int
initValue (Object entity) | isNothing (getMaybePair entity (ID "value")) = Nothing
                          | otherwise = Just (toInt (getValue (getPair entity (ID "value"))))

------------------ Initializing actions ------------------
initActions :: JSON -> [Action]
initActions (Actions []) = []
initActions actions = toActions actions
