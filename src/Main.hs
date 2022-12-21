import GameRenderer
import Parser
import Datastructures
import LevelLoader
import GameLogica
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import GHC.IO (unsafePerformIO)

main :: IO ()
main = play window black fps (unsafePerformIO (initGame "level3")) renderGame handleInput step

step :: Float -> Game -> Game
step _ b = b
