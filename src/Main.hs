import GameRenderer
import Parser
import Datastructures
import LevelLoader
import GameLogica
import StartScreen
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import GHC.IO (unsafePerformIO)

main :: IO ()
main = play window black fps emptyGame renderState handleInput step

handleInput :: Event -> Game -> Game
handleInput ev game | (state game) == Selecting = handleLevelSelectInput ev game
                    | (state game) == Playing = handleLevelInput ev game
                    | (state game) == Completed = handleEndInput ev game
                    | otherwise = game

renderState :: Game -> Picture
renderState game | (state game) == Selecting = renderStartScreen game
                 | (state game) == Playing = renderGame game
                 | (state game) == Completed = renderEndScreen

step :: Float -> Game -> Game
step _ b = gameState b

testGame :: IO Game -> Game
testGame = unsafePerformIO
