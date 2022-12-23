import Test.Hspec
import GHC.IO (unsafePerformIO)

import Functions
import GameLogica
import LevelLoader
import Datastructures

genericItem :: String -> GameItem
genericItem id = GameItem id 0 0 "name" "description" Infinite 0 []

genericPlayer :: Player
genericPlayer = Player 0 [genericItem (show x) | x <- [0..8]] 0 0 Datastructures.Right

gameLevel1 :: Game
{-# NOINLINE gameLevel1 #-}
gameLevel1 = unsafePerformIO (initGame 0 "level1")

gameLevel2 :: Game
{-# NOINLINE gameLevel2 #-}
gameLevel2 = unsafePerformIO (initGame 0 "level2")

main :: IO ()
main = hspec $ do 
    it "Test action conditions" $ do
        inventoryFull genericPlayer  `shouldBe` True
        inventoryContains genericPlayer (itemId (genericItem "0")) `shouldBe` True
        inventoryContains genericPlayer (itemId (genericItem "9")) `shouldBe` False

    it "Test if actions get detected in game" $ do
        getActions gameLevel1 `shouldBe` []
        length (getAllActions gameLevel2) `shouldBe` 2


    it "Testing player movement" $ do
        canMoveTo (0,0) gameLevel1 `shouldBe` False
        canMoveTo (2,1) gameLevel1 `shouldBe` True
        (playerX (player (movePlayer (1,0) gameLevel1)), playerY (player (movePlayer (1,0) gameLevel1))) `shouldBe` (2,1)
        (playerX (player (movePlayer (0,1) gameLevel1)), playerY (player (movePlayer (0,1) gameLevel1))) `shouldBe` (1,1)

    it "Testing item detection" $ do
        senseItems gameLevel1 `shouldBe` []
        length (senseItems gameLevel2) `shouldBe` 1
        length (inventory (player (retrieveItem gameLevel2 "key"))) `shouldBe` 1
