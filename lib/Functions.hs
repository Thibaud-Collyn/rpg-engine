module Functions where

import Datastructures

-- not:: Bool -> Bool is already defined in Prelude

inventoryFull :: Player -> Bool
inventoryFull player = length (inventory player) == 9

inventoryContains :: Player -> String -> Bool
inventoryContains player objId = not (null [x | x <- inventory player, itemId x == objId])
