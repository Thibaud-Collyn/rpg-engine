module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO

--FIXME: make separate Datastructures file and import it here

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

data Function = Function { id::ID, args::[Argument]} deriving (Show, Eq)
data Action = Action { conditions::[Function], action::Function} deriving (Show, Eq)

data JSON = Number Int | String String | Actions[Action] | Array [JSON] | Object [Pair] | Layout [TileLine] | UseTimes UseTimes 
    deriving (Show, Eq)

whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\n")

parseJSON :: Parser JSON
parseJSON = parseString <|> parseNumber <|> parseArray <|> parseObject <|> parseLayout <|> parseLevel -- <|> parseUseTimes <|> parseAction <|> parseDirection

parseString :: Parser JSON
parseString = String <$> (char '"' *> many1 (letter <|> space) <* char '"')

parseNumber :: Parser JSON
parseNumber = Number . read <$> many1 digit <* whitespace

parseArray :: Parser JSON
parseArray = Array <$> (char '[' *> sepBy parseJSON (char ',') <* char ']')

parseObject :: Parser JSON
parseObject = Object <$> (whitespace *> char '{' *> whitespace *> sepBy parsePair (char ',') <* whitespace <* char '}' <* whitespace)

parseObjID :: Parser ID
parseObjID = ID <$> (whitespace *> many1 letter <* whitespace)

parsePair :: Parser Pair
parsePair = do
    pairType <- whitespace >> parseObjID
    if pairType == ID "layout" then 
        Pair pairType <$> (whitespace >> char ':' >> parseLayout)
        -- else if pairType == ID "useTimes" then
        --     Pair pairType <$> (whitespace >> char ':' >> parseUseTimes) <* whitespace
        -- else if pairType == ID "actions" then
        --     Pair pairType <$> (whitespace >> char ':' >> parseAction) <* whitespace
        -- else if pairType == ID "direction" then
        --     Pair pairType <$> (whitespace >> char ':' >> parseDirection) <* whitespace
        else Pair pairType <$> (char ':' >> whitespace >> parseJSON <* whitespace)

parseLevel:: Parser JSON
parseLevel = Object <$> (whitespace *> many1 parsePair <* whitespace)

parseLayout :: Parser JSON
parseLayout = Layout <$> (whitespace *> char '{' *> whitespace *> char '|' *> whitespace *> sepBy parseTileLine (char '|') 
    <* whitespace <* char '}' <* whitespace)

parseTileLine :: Parser TileLine
parseTileLine = TileLine <$> (whitespace *> sepBy parseTile (char ' ') <* whitespace)

parseTile :: Parser Tile
parseTile = parseEmpty <|> parseWall <|> parseFloor <|> parseStart <|> parseEnd

parseEmpty :: Parser Tile
parseEmpty = char 'x' *> return Parser.Empty

parseWall :: Parser Tile
parseWall = char '*' *> return Wall

parseFloor :: Parser Tile
parseFloor = char '.' *> return Floor

parseStart :: Parser Tile
parseStart = char 's' *> return Start

parseEnd :: Parser Tile
parseEnd = char 'e' *> return End

--TODO: parseUseTimes is not yet implemented
parseUseTimes :: Parser JSON
parseUseTimes = undefined

--TODO: parseAction is not yet implemented
parseAction :: Parser JSON
parseAction = undefined

parseDirection :: Parser JSON
parseDirection = undefined

parseConfigFile :: JSON
parseConfigFile = undefined

parseTest :: String -> IO ()
parseTest level = do
    withFile level ReadMode (\handle -> do
        contents <- hGetContents handle
        let parsed = (parse parseJSON "error" (contents))
        case parsed of
            Prelude.Left err -> error $ show err
            Prelude.Right json -> putStr (show json)
        )