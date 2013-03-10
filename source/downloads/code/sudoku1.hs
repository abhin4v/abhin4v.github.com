{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Sudoku where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', intersperse, intercalate)
import Data.List.Split (chunksOf)
import Control.Monad (foldM, guard)
data Digit = ONE | TWO | TRE | FOR | FIV | SIX | SVN | EGT | NIN
             deriving (Eq, Ord, Enum)

instance Show Digit where
  show digit = show $ fromEnum digit + 1

data Cell = Cell { cellIdx :: Int, cellVals :: S.Set Digit }
            deriving (Eq)

instance Show Cell where
  show Cell{..} = "<" ++ show cellIdx ++ " " ++ show (S.toList cellVals) ++">"

type Board = M.Map Int Cell

emptyBoard :: Board
emptyBoard = foldl' (\m i -> M.insert i (Cell i $ S.fromList [ONE .. NIN]) m) 
                    M.empty [0 .. 80]
readBoard :: String -> Maybe Board
readBoard str = do
  guard $ length str == 81
  foldM (\board (i, chr) -> do
            guard $ chr == '.' || (chr `S.member` S.fromList ['1' .. '9'])
            let cellVals = if chr == '.'
                             then S.fromList [ONE .. NIN]
                             else S.singleton $ toEnum $ digitToInt chr - 1
            return $ M.insert i (Cell i cellVals) board)
        emptyBoard 
        $ zip [0 .. 80 ] str
showBoard :: Board -> String
showBoard =
  map (\Cell{..} ->
          if S.size cellVals == 1 
            then intToDigit . (+ 1) . fromEnum . head . S.toList $ cellVals
            else '.')
  . M.elems

asciiShowBoard :: Board -> String
asciiShowBoard =
  (\t -> border ++ "\n" ++ t ++ border ++ "\n")
  . unlines . intercalate [border] . chunksOf 3
  . map ((\r -> "| " ++ r ++ " |")
         . intercalate " | " . map (intersperse ' ') . chunksOf 3)
  . chunksOf 9
  . showBoard
  where border = "+-------+-------+-------+"
