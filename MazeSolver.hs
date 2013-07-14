--This is free and unencumbered software released into the public domain.
--
--Anyone is free to copy, modify, publish, use, compile, sell, or
--distribute this software, either in source code form or as a compiled
--binary, for any purpose, commercial or non-commercial, and by any
--means.
--
--In jurisdictions that recognize copyright laws, the author or authors
--of this software dedicate any and all copyright interest in the
--software to the public domain. We make this dedication for the benefit
--of the public at large and to the detriment of our heirs and
--successors. We intend this dedication to be an overt act of
--relinquishment in perpetuity of all present and future rights to this
--software under copyright law.
--
--THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
--OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
--ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
--OTHER DEALINGS IN THE SOFTWARE.
--
--For more information, please refer to <http://unlicense.org/>

type Cell = (Int, Int)
data Maze = Maze Int Int [[Bool]]

maybeAppend :: a -> Maybe [a] -> Maybe [a]
maybeAppend _ Nothing         = Nothing
maybeAppend val (Just others) = Just (val : others)

getPathDriver :: Cell -> Cell -> [Cell] -> Maze -> Maybe [Cell]
getPathDriver current end seen (Maze maxX maxY mazeMap) 
     | current == end                                          = Just [current]
     | isWall                                                  = Nothing
     | currX - 1 >= 0   && notSeen west  && wResult /= Nothing = maybeAppend current wResult
     | currY - 1 >= 0   && notSeen north && nResult /= Nothing = maybeAppend current nResult
     | currX + 1 < maxX && notSeen east  && eResult /= Nothing = maybeAppend current eResult
     | currY + 1 < maxY && notSeen south && sResult /= Nothing = maybeAppend current sResult
     | otherwise                                               = Nothing
     where notSeen :: Cell -> Bool
           notSeen cell = not (elem cell seen)
           currX    = fst current
           currY    = snd current
           isWall   = ((mazeMap !! currY ) !! currX ) 
           west     = (currX - 1, currY)
           north    = (currX, currY - 1)
           east     = (currX + 1, currY)
           south    = (currX, currY + 1)
           origMaze = (Maze maxX maxY mazeMap)
           newSeen  = current:seen
           wResult  = getPathDriver west  end newSeen origMaze
           nResult  = getPathDriver north end newSeen origMaze 
           eResult  = getPathDriver east  end newSeen origMaze 
           sResult  = getPathDriver south end newSeen origMaze

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
