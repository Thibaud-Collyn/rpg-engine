import GameRenderer
import Parser
import Datastructures
import LevelLoader
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import GHC.IO (unsafePerformIO)

main :: IO ()
main = play window black fps (unsafePerformIO (initGame "level3")) renderGame handleInput step

handleInput :: Event -> Game -> Game
handleInput ev game = game

step :: Float -> Game -> Game
step _ b = b
