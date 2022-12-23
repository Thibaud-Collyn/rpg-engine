module Datastructures where

import System.IO
import Data.Maybe (fromJust, isNothing)

-------------------- Parser/JSON data structures --------------------
data ID = ID String deriving (Show, Eq)

data Pair = Pair ID JSON deriving (Show, Eq)

data TileLine = TileLine [Tile] deriving (Show, Eq)

data Tile = Empty | Wall | Floor | Start | End 
    deriving (Show, Eq)

data Direction = Up | Down | Left | Right 
    deriving (Show, Eq)

data UseTimes = Infinite | TimesUsed Int 
    deriving (Show, Eq)

data Argument = TargetId ID | ArgFunction Function 
    deriving (Show, Eq)

data JSON = Number Int | String String | Actions[Action] | Array [JSON] | Object [Pair] | Layout [TileLine] | Direction Direction | UseTimes UseTimes 
    deriving (Show, Eq)

-------------------- Game data structures --------------------
data GameState = Selecting | Playing | Completed
    deriving (Show, Eq)

data Game = Game { player::Player, levels::[Level], currentLevel::Int, state::GameState, selector::Int} deriving (Show, Eq)

data Player = Player {playerHP::Int, inventory::[GameItem], playerX::Int, playerY::Int, playerDirection::Direction} deriving (Show, Eq)

data Level = Level { layout::[TileLine], items::[GameItem], entities::[Entity]} deriving (Show, Eq)

data GameItem = GameItem { itemId::String, itemX::Int, itemY::Int, itemName::String, itemDescription::String, itemUseTimes::UseTimes, itemValue::Int, itemActions::[Action]} deriving (Show, Eq)

data Entity = Entity { entityId::String, entityX::Int, entityY::Int, entityName::String, entityDescription::String, entityDirection::Maybe Direction, entityHP::Maybe Int, entityValue::Maybe Int, entityActions::[Action]} deriving (Show, Eq)

data Function = Function { id::String, args::[Argument]} deriving (Show, Eq)

data Action = Action { conditions::[Function], action::Function} deriving (Show, Eq)

-------------------- Functions for datastructures --------------------
-- Returns the value of a pair that may or may not exist with a given key
getMaybePair :: [Pair] -> ID -> Maybe Pair
getMaybePair [] _ = Nothing
getMaybePair (x:xs) id = if (getID x) == id then Just x else getMaybePair xs id

-- Returns the value of a pair that must exist with a given key
getPair :: [Pair] -> ID -> Pair
getPair pairs id = fromJust (getMaybePair pairs id)

-- Returns the id of a pair
getID :: Pair -> ID
getID (Pair id _) = id

-- Returns the value of a pair
getValue :: Pair -> JSON
getValue (Pair _ value) = value

-- Returns int value of json number
toInt :: JSON -> Int
toInt (Number n) = n

-- Returns string value of json string
toStr :: JSON -> String
toStr (String s) = s

idToStr :: ID -> String
idToStr (ID s) = s

-- Returns Layout as array of TileLines
layoutToArray :: JSON -> [TileLine]
layoutToArray (Layout layout) = layout

-- Returns UseTimes of JSON
toUseTimes :: JSON -> UseTimes
toUseTimes (UseTimes useTimes) = useTimes

-- Returns Direction of JSON
toDirection :: JSON -> Direction
toDirection (Direction direction) = direction

toActions :: JSON -> [Action]
toActions (Actions actions) = actions

tileLineToArray :: TileLine -> [Tile]
tileLineToArray (TileLine tileLine) = tileLine

-- Returns an ampty game used in the level selector
emptyGame :: Game
emptyGame = Game { player = (Player 1 [] 0 0 Datastructures.Right), levels = [], currentLevel = 0, state = Selecting, selector = 0}

-- Used for testing
emptyGameCompleted :: Game
emptyGameCompleted = Game { player = (Player 1 [] 0 0 Datastructures.Right), levels = [], currentLevel = 0, state = Completed, selector = 0}

argumentToId :: Argument -> String
argumentToId (TargetId id) = idToStr id

argumentToFunction :: Argument -> Function
argumentToFunction (ArgFunction function) = function

useTimesToInt :: UseTimes -> Int
useTimesToInt (TimesUsed n) = n
