type Cell = (Int, Int)
data Maze = Maze Int Int [[Bool]]

maybeAppend :: a -> Maybe [a] -> Maybe [a]
maybeAppend _ Nothing         = Nothing
maybeAppend val (Just others) = Just (val : others)

getPathDriver :: Cell -> Cell -> [Cell] -> Maze -> Maybe [Cell]
getPathDriver current end seen (Maze maxX maxY mazeMap) 
     | current == end                                                       = Just [current]
     | isWall                                                               = Nothing
     | currX - 1 >= 0   && notSeen (currX - 1, currY) && wResult /= Nothing = maybeAppend current wResult
     | currY - 1 >= 0   && notSeen (currX, currY - 1) && nResult /= Nothing = maybeAppend current nResult
     | currX + 1 < maxX && notSeen (currX + 1, currY) && eResult /= Nothing = maybeAppend current eResult
     | currY + 1 < maxY && notSeen (currX, currY + 1) && sResult /= Nothing = maybeAppend current sResult
     | otherwise                                                            = Nothing
     where notSeen :: Cell -> Bool
           notSeen cell = not (elem cell seen)
           currX   = fst current
           currY   = snd current
           isWall  = ((mazeMap !! currY ) !! currX ) 
           wResult = getPathDriver (currX - 1, currY) end (current:seen) (Maze maxX maxY mazeMap)
           nResult = getPathDriver (currX, currY - 1) end (current:seen) (Maze maxX maxY mazeMap)
           eResult = getPathDriver (currX + 1, currY) end (current:seen) (Maze maxX maxY mazeMap)
           sResult = getPathDriver (currX, currY + 1) end (current:seen) (Maze maxX maxY mazeMap)

getPath :: Cell -> Cell -> Maze -> Maybe [Cell]
getPath start end maze = getPathDriver start end [] maze

toCell :: [String] -> Cell
toCell [y, x] = ((read x :: Int), (read y :: Int))

formatCell :: Cell -> [Cell] -> Cell -> Cell -> Bool -> Char
formatCell position path start end isWall 
      | isWall             = '#'
      | position == start  = 'S'
      | position == end    = 'E'
      | elem position path = 'X'
      | otherwise          = ' '

formatRow :: Int -> [Cell] -> Cell -> Cell -> [Bool] -> String
formatRow row path start end mazeData = 
       map (\x -> formatCell ((fst x), row) path start end (snd x)) zipPos
       where zipPos = zip [0,1..] mazeData

formatOutputDriver :: [Cell] -> Cell -> Cell -> [[Bool]] -> [String]
formatOutputDriver _ _ _ _ [] = []
formatOutputDriver path start end mazeData = 
       map (\x -> formatRow (fst x) path start end (snd x)) zipPos
       where zipPos = zip [0,1..] mazeData
       

formatOutput :: Maybe [Cell] -> Cell -> Cell -> Maze -> [String]
formatOutput Nothing _ _ _ = ["Cannot find path"] 
formatOutput (Just path) start end (Maze _ _ mazeData) = "OUTPUT:" : formatOutputDriver path start end mazeData

main :: IO()
main = do 
    dimsStr  <- getLine
    startStr <- getLine
    endStr   <- getLine
    mazeStr  <- getContents
    let dims     = toCell $ words dimsStr
    let start    = toCell $ words startStr
    let end      = toCell $ words endStr
    let maxX     = fst dims
    let maxY     = snd dims
    let mazeData = map (\x -> map (== "1") x) $ map words $ lines mazeStr
    let mazeObj  = (Maze maxX maxY mazeData)
    let path     = getPath start end mazeObj
    let output   = unlines $ formatOutput path start end mazeObj
    putStr output
