module GameRenderer where

import Datastructures
import LevelLoader
import TextureLoader
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import System.IO
import Data.Maybe (fromJust, isNothing)

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
renderGame game =  renderLevel ((levels game)!!currentLevel game) (player game)

-------------------- Render level --------------------
renderLevel :: Level -> Player -> Picture
renderLevel currentLevel currentPlayer = pictures [renderGui currentLevel currentPlayer, renderLayout (layout currentLevel), renderItems (items currentLevel), renderEntities (entities currentLevel), renderPlayer currentPlayer]

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
 
renderPlayer :: Player -> Picture
renderPlayer player = translate (fromIntegral ((playerX player)*tileOffset)) (fromIntegral ((playerY player)*tileOffset)) (playerKnight getGameTextures)

renderEntities :: [Entity] -> Picture
renderEntities entities = pictures [renderEntity entity | entity <- entities]

renderEntity :: Entity -> Picture
renderEntity entity = (translate (fromIntegral (((entityX entity)+1)*tileOffset)) (fromIntegral (((entityY entity)+1)*tileOffset)) (rotateInDirection (entityDirection entity) (textureByID (entityId entity) getGameTextures)))

rotateInDirection :: Maybe Direction -> Picture -> Picture
rotateInDirection dir pic | isNothing dir = pic
                          | fromJust dir == Datastructures.Up = pic
                          | fromJust dir == Datastructures.Down = rotate 180.0 pic
                          | fromJust dir == Datastructures.Left = rotate 270.0 pic
                          | fromJust dir == Datastructures.Right = rotate 90.0 pic
                          | otherwise = pic

-------------------- Render GUI --------------------
renderGui :: Level -> Player -> Picture
renderGui lvl player = pictures [renderBackground, renderItemBar (inventory player)]

renderBackground :: Picture
renderBackground = scale 0.7 0.9 (background getGuiTextures)

renderItemBar :: [GameItem] -> Picture
renderItemBar items = renderBar

--TODO: fix bar assets
renderBar :: Picture
renderBar = translate (fromIntegral (5*tileOffset)) (fromIntegral (-4*tileOffset)) (itemBar getGuiTextures)