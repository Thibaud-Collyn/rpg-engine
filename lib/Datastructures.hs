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
data Game = Game { player::Player, levels::[Level], currentLevel::Int} deriving (Show, Eq)

data Player = Player {playerHP::Int, inventory::[GameItem], playerX::Int, playerY::Int, playerDirection::Direction} deriving (Show, Eq)

data Level = Level { layout::[TileLine], items::[GameItem], entities::[Entity]} deriving (Show, Eq)

data GameItem = GameItem { itemId::ID, itemX::Int, itemY::Int, itemName::String, itemDescription::String, itemUseTimes::UseTimes, itemActions::[Action]} deriving (Show, Eq)

data Entity = Entity { entityId::ID, entityX::Int, entityY::Int, entityName::String, entityDescription::String, entityDirection::Maybe Direction, entityHP::Maybe Int, entityValue::Maybe Int, entityActions::[Action]} deriving (Show, Eq)

data Function = Function { id::ID, args::[Argument]} deriving (Show, Eq)

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