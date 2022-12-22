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

itemSlotOffset :: Int
itemSlotOffset = 60

window :: Display
window = InWindow "Haskell Adventure" (810, 600) windowPosition

--FIXME: render game and gui
renderGame :: Game -> Picture
renderGame game =  renderLevel (levels game !! currentLevel game) (player game)

-------------------- Render level --------------------
renderLevel :: Level -> Player -> Picture
renderLevel currentLevel currentPlayer = pictures [renderGui currentLevel currentPlayer, renderLayout (layout currentLevel), renderItems (items currentLevel), renderEntities (entities currentLevel), renderPlayer currentPlayer]

renderLayout :: [TileLine] -> Picture
renderLayout layout = pictures [renderTileLine (layout!!y) y | y <- [0..length layout - 1]]

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
renderGui lvl player = pictures [renderBackground, renderItemBar (inventory player), renderHP player]

renderBackground :: Picture
renderBackground = scale 0.7 0.9 (background getGuiTextures)

renderItemBar :: [GameItem] -> Picture
renderItemBar items = pictures [renderBar, renderItemSlots, renderInventory items, renderActionBar]

renderBar :: Picture
renderBar = translate (fromIntegral (2*tileOffset)) (fromIntegral (-5*tileOffset)) (itemBar getGuiTextures)

renderItemSlots :: Picture
renderItemSlots = pictures [translate (fromIntegral (-175 + (x*itemSlotOffset))) (fromIntegral (-5*tileOffset)) image | x <- [0..8]] where image = itemSlot getGuiTextures

renderInventory :: [GameItem] -> Picture
renderInventory items = pictures [translate (fromIntegral (-175 + (x*itemSlotOffset))) (fromIntegral (-5*tileOffset)) (textureByID (itemId (items!!x)) getGameTextures) | x <- [0..(length items) -1]]

renderActionBar :: Picture
renderActionBar = translate (-210.0) 90.0 (scale 0.7 0.9 (actionBar getGuiTextures))

renderHP :: Player -> Picture
renderHP player = translate (-350.0) (fromIntegral (-5*tileOffset)) (color white (scale 0.15 0.15 (text ("HP = " ++ (show (playerHP player))))))

--TODO: render all actions
