module Functions where

import Datastructures
import Data.Maybe (fromJust)

-- not:: Bool -> Bool is already defined in Prelude

--------------------- Conditionals ---------------------
inventoryFull :: Player -> Bool
inventoryFull player = length (inventory player) == 9

inventoryContains :: Player -> String -> Bool
inventoryContains player objId = inventory player `contains` objId

contains :: [GameItem] -> String -> Bool
contains [] _ = False
contains (x:xs) objId = objId == itemId x || contains xs objId

--------------------- Retrieve item ---------------------
retrieveItem :: Game -> String -> Game
retrieveItem game id = (addToInventory game item) {levels = (removeFromLevel game item)} where item = head [i | i <- getItems (levels game !! currentLevel game) id, inRange (playerX (player game), playerY (player game)) (itemX i, itemY i)]

addToInventory :: Game -> GameItem -> Game
addToInventory game item = game {player = (player game) {inventory = (inventory (player game)) ++ [item]}}

removeFromLevel :: Game -> GameItem -> [Level]
removeFromLevel game item = [if i == (currentLevel game) then removeItem ((levels game)!!i) item else (levels game)!!i | i <- [0.. length (levels game)-1]]

removeItem :: Level -> GameItem -> Level
removeItem level item = level {items = [i | i <- items level, i /= item]}

inRange :: (Int, Int) -> (Int, Int) -> Bool
inRange (x1, y1) (x2, y2) | x1-1 == x2 && y1-1 == y2 = True
                          | x1 == x2 && y1-1 == y2 = True
                          | x1-2 == x2 && y1-1 == y2 = True
                          | x1-1 == x2 && y1 == y2 = True
                          | x1-1 == x2 && y1-2 == y2 = True
                          | otherwise = False

isEntityInRange :: Player -> Entity -> Bool
isEntityInRange player entity = inRange (playerX player, playerY player) (entityX entity, entityY entity)

getEntity :: Level -> String -> [Entity]
getEntity level id = [entity | entity <- entities level, entityId entity == id]

getItems :: Level -> String -> [GameItem]
getItems level id = [item | item <- items level, itemId item == id]

useItem :: Game -> String -> Game
useItem game item = useTimesDecrease item (game {levels = replaceNth (currentLevel game) lvl {entities = [if e == (findLockedDoor game item) then e {entityValue = Just 0} else e |e <- entities lvl]} (levels game)}) where lvl = levels game !! currentLevel game

-- Finds a locked door that uses key with given id and is in range of the player
findLockedDoor :: Game -> String -> Entity
findLockedDoor game id = head [entity | entity <- entities (levels game !! currentLevel game), (getKeyId entity) == id && inRange (playerX (player game), playerY (player game)) (entityX entity, entityY entity)]

getKeyId :: Entity -> String
getKeyId entity = head [argumentToId (head (args (action a))) | a <- entityActions entity, (Datastructures.id (action a)) == "useItem"]

-- replaces nth element in a list with a new value
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


---------------- Increse player HP ----------------
increasePlayerHP :: Game -> String -> Game
increasePlayerHP game id = useTimesDecrease id (game {player = (player game) {playerHP = (playerHP (player game)) + (itemValue (getInventoryItem game id))}})

------------------- Decrease HP -------------------
decreaseHP :: Game -> String -> String -> Game
decreaseHP game entity item = useTimesDecrease item game {levels = replaceNth (currentLevel game) (decreaseHPLevel (player game) (levels game !! currentLevel game) entity (itemValue (getInventoryItem game item))) (levels game)}

decreaseHPLevel :: Player -> Level -> String -> Int -> Level
decreaseHPLevel player level entity val = level {entities = [if ent == targetE then decreaseEntityHP ent val else ent | ent <- (entities level)]} where targetE = head [e | e <- (getEntity level entity), isEntityInRange player e]

decreaseEntityHP :: Entity -> Int -> Entity
decreaseEntityHP entity val = entity {entityHP = Just (fromJust(entityHP entity) - val)}

------------------- Item use times handeling -------------------
useTimesDecrease :: String -> Game -> Game
useTimesDecrease id game | itemUseTimes (getInventoryItem game id) == Infinite = game
                         | useTimesToInt (itemUseTimes (getInventoryItem game id)) == 1 = removeFromInventory game id
                         | otherwise = game {player = (player game) {inventory = [if itemId item == id then item {itemUseTimes = TimesUsed ((useTimesToInt (itemUseTimes item)) - 1)} else item | item <- inventory (player game)]}}

getInventoryItem :: Game -> String -> GameItem
getInventoryItem game id = head [item | item <- inventory (player game), itemId item == id]

removeFromInventory :: Game -> String -> Game
removeFromInventory game id = game {player = (player game) {inventory = [item | item <- inventory (player game), itemId item /= id]}}

------------------- Leave Function -------------------
leave :: Game -> Game
leave game = game {player = (player game) {playerHP = (playerHP (player game)) - 10}}
