module GameRenderer where

import Datastructures
import LevelLoader
import TextureLoader
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import System.IO

fps :: Int
fps = 60

windowPosition :: (Int, Int)
windowPosition = (200, 200)

tileOffset :: Int
tileOffset = 32

window :: Display
window = InWindow "Haskell Adventure" (900, 600) windowPosition

--FIXME: render game and gui
renderGame :: Game -> Picture
renderGame game =  renderLevel ((levels game)!!0)

renderBackground :: Picture
renderBackground = scale 0.7 0.9 (background getGuiTextures)

--FIXME: render entities
renderLevel :: Level -> Picture
renderLevel currentLevel = pictures [renderBackground, renderLayout (layout currentLevel), renderItems (items currentLevel)]

renderLayout :: [TileLine] -> Picture
renderLayout layout = pictures [renderTileLine ((reverse layout)!!y) y | y <- [0..length layout - 1]]

renderTileLine :: TileLine -> Int -> Picture
renderTileLine (TileLine tiles) y = pictures [translate (fromIntegral (x*tileOffset)) (fromIntegral (y*tileOffset)) (renderTile (tiles!!x))| x <- [0..length tiles - 1]]

renderTile :: Tile -> Picture
renderTile tile | tile == Wall = wall getGameTextures
                | tile == Floor = floorTile getGameTextures
                | tile == Start = startTile getGameTextures
                | tile == End = endTile getGameTextures
                | otherwise = Blank

renderItems :: [GameItem] -> Picture
renderItems items = pictures [renderItem item | item <- items]

renderItem :: GameItem -> Picture
renderItem item = translate (fromIntegral ((x+1)*tileOffset)) (fromIntegral ((y+1)*tileOffset)) (textureByID (itemId item) getGameTextures) where x = itemX item
                                                                                                                                                  y = itemY item
