{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Sudoku where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', intersperse, intercalate, sortBy, nub)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Control.Monad (foldM, guard)
data Digit = ONE | TWO | TRE | FOR | FIV | SIX | SVN | EGT | NIN
             deriving (Eq, Ord, Enum)

instance Show Digit where
  show digit = show $ fromEnum digit + 1

allDigits = S.fromList [ONE .. NIN]

data Cell = Cell { cellIdx :: Int, cellVals :: S.Set Digit }
            deriving (Eq, Ord)

instance Show Cell where
  show Cell{..} = "<" ++ show cellIdx ++ " " ++ show (S.toList cellVals) ++">"

newtype Board = Board (M.Map Int Cell)
                deriving (Eq, Ord)

boardCells :: Board -> [Cell]
boardCells (Board ixMap) = M.elems ixMap

cellAt :: Board -> Int -> Maybe Cell
cellAt (Board ixMap) idx = M.lookup idx ixMap

updateBoard :: Board -> Cell -> Board
updateBoard (Board ixMap) cell@Cell{..} = Board (M.insert cellIdx cell ixMap)

emptyBoard :: Board
emptyBoard = 
  Board $ foldl' (\m i -> M.insert i (Cell i allDigits) m) M.empty [0 .. 80]
readBoard :: String -> Maybe Board
readBoard str = do
  guard $ length str == 81
  foldM (\board (i, chr) -> do
            guard $ chr == '.' || (chr `S.member` S.fromList ['1' .. '9'])
            let cellVals = if chr == '.'
                             then allDigits
                             else S.singleton $ toEnum $ digitToInt chr - 1
            return $ updateBoard board (Cell i cellVals))
        emptyBoard 
        $ zip [0 .. 80 ] str
showBoard :: Board -> String
showBoard =
  map (\Cell{..} ->
          if S.size cellVals == 1 
            then intToDigit . (+ 1) . fromEnum . head . S.toList $ cellVals
            else '.')
  . boardCells

asciiShowBoard :: Board -> String
asciiShowBoard =
  (\t -> border ++ "\n" ++ t ++ border ++ "\n")
  . unlines . intercalate [border] . chunksOf 3
  . map ((\r -> "| " ++ r ++ " |")
         . intercalate " | " . map (intersperse ' ') . chunksOf 3)
  . chunksOf 9
  . showBoard
  where border = "+-------+-------+-------+"

instance Show Board where
  show = showBoard
data BoardState = SOLVED | INCOMPLETE | INVALID
                  deriving (Eq, Show)

boardState :: Board -> BoardState
boardState board
  | any (\Cell{..} -> S.size cellVals /= 1) $ boardCells board = INCOMPLETE
  | any isUnitInvalid units = INVALID
  | otherwise = SOLVED
  where
    isUnitInvalid unitCells = 
      (S.fromList . map (head . S.toList . cellVals) $ unitCells) /= allDigits

    units = map (map (fromJust . cellAt board)) unitIxs

unitIxs   = rowIxs ++ columnIxs ++ blockIxs
rowIxs    = map (\i -> [i * 9 .. i * 9 + 8]) [0..8]
columnIxs = map (\i -> take 9 [i, i + 9 ..]) [0..8]
blockIxs  = 
  concatMap (\(row1:row2:row3:_) ->
                zipWith3 (\blockRow1 blockRow2 blockRow3 ->
                             blockRow1 ++ blockRow2 ++ blockRow3) 
                         row1 row2 row3)
  . chunksOf 3 . map (chunksOf 3) $ rowIxs
dfs :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> [a]
dfs start getNext isGoal = go start S.empty
  where 
    go node visited
      | isGoal node = [node]
      | S.member node visited = []
      | otherwise = concatMap (\nextNode ->
                               go nextNode (S.insert node visited))
                              (getNext node)

dfsSolver :: Board -> [Board]
dfsSolver board = dfs board nextBoards ((== SOLVED) . boardState)
  where
    nextBoards board = 
      map (updateBoard board) 
      . concatMap (\Cell{..} -> map (Cell cellIdx . S.singleton) . S.toList $ cellVals)
      . sortBy (comparing (S.size . cellVals)) 
      . filter ((/= 1) . S.size . cellVals) 
      . boardCells
      $ board

