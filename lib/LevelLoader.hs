module LevelLoader where

import Datastructures
import Parser
import Text.Parsec

initGame :: String -> IO Game
initGame levelName = do
    levelFile <- readFile ("levels/" ++ levelName ++ ".txt")
    let levelJson = parse parseJSON "Could not parse file" levelFile
    case levelJson of
        Prelude.Left err -> error $ show err
        Prelude.Right levelJson -> return $ initGameFromJson levelJson

initGameFromJson :: JSON -> Game
initGameFromJson (Object pairs) = Game (initPlayer (getValue (getPair pairs (ID "player")))) (initLevels (getValue ((getPair pairs (ID "levels"))))) 0

--FIXME: might be better to give a JSON object to initPlayer and initLevels(getValue in initGameFromJson!)
initPlayer :: JSON -> Player
initPlayer (Object pairs) = Player (toInt (getValue (getPair pairs (ID "hp")))) (initItems (getValue (getPair pairs (ID "inventory")))) 0 0 Datastructures.Right

initLevels :: JSON -> [Level]
initLevels (Object pairs) = undefined

initItems :: JSON -> [GameItem]
initItems (Array []) = []
initItems (Object items) = undefined
