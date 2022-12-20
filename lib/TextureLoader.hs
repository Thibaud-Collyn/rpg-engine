module TextureLoader where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)

data GameTextures = GameTextures {
    wall :: Picture,
    floorTile :: Picture,
    startTile :: Picture,
    endTile :: Picture,
    doorLocked :: Picture,
    doorUnlocked :: Picture,
    player :: Picture,
    key :: Picture,
    sword :: Picture,
    dagger :: Picture,
    potion :: Picture,
    demon :: Picture
} deriving (Show, Eq)


data GuiTextures = GuiTextures {
    background :: Picture
}

getGameTextures :: GameTextures
getGameTextures = GameTextures {
    wall = loadPicture "game" "wall_horizontal",
    floorTile = loadPicture "game" "floor_1",
    startTile = loadPicture "game"  "start_tile",
    endTile = loadPicture "game"  "stair_nextlevel",
    doorLocked = loadPicture "game" "door_locked",
    doorUnlocked = loadPicture "game" "door_open",
    player = loadPicture "game" "knight",
    key = loadPicture "game" "key_silver",
    sword = loadPicture "game" "sword",
    dagger = loadPicture "game" "dagger.",
    potion = loadPicture "game" "potion",
    demon = loadPicture "game" "demon"
}

getGuiTextures :: GuiTextures
getGuiTextures = GuiTextures {
    background = loadPicture "gui" "gui_background"
}

textureByID :: String -> GameTextures -> Picture
textureByID id textures | id == "key" = key textures
                        | id == "sword" = sword textures
                        | id == "dagger" = dagger textures
                        | id == "potion" = potion textures
                        | id == "demon" = demon textures
                        | id == "door" = doorLocked textures
                        | otherwise = Blank

loadPicture :: String -> String -> Picture
loadPicture folder asset = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ folder ++ "/" ++ asset ++ ".png")))