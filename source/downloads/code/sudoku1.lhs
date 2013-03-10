
---
layout: post
title: "Sudoku and Haskell: A Sudoku Solver"
date: 2013-03-09 23:35
comments: true
categories: programming haskell sudoku puzzle
published: false
---

Sudoku a popular number placement puzzle. It is played on a 9 by 9 grid where the objective is to
fill each cell in the grid with digits from 1 to 9 such that each column, each row and each 3 by 3
sub-grids (called blocks) have every digit from 1 to 9. The puzzle starts with some cells pre-filled 
and the player has to fill the rest to reach the solution. Since each unit (column, row or block) has
9 cells and has to be filled with all 9 digits - 1 to 9 - there cannot be any duplicates in a unit.

In this post we look at how to solve a Sudoku with Haskell. 

<!-- more -->

The code in this post has dependencies on the [`split`][1] package from Hackage.

> {-# LANGUAGE BangPatterns, RecordWildCards #-}
> 
> module Sudoku where
> 
> import qualified Data.Set as S
> import qualified Data.Map as M
> import Data.Char (digitToInt, intToDigit)
> import Data.List (foldl', intersperse, intercalate)
> import Data.List.Split (chunksOf)
> import Control.Monad (foldM, guard)


Basic Setup
-----------

Now that the imports are out of the way let's setup the basic functionalities.
 
> data Digit = ONE | TWO | TRE | FOR | FIV | SIX | SVN | EGT | NIN
>              deriving (Eq, Ord, Enum)
> 
> instance Show Digit where
>   show digit = show $ fromEnum digit + 1
> 
> data Cell = Cell { cellIdx :: Int, cellVals :: S.Set Digit }
>             deriving (Eq)
> 
> instance Show Cell where
>   show Cell{..} = "<" ++ show cellIdx ++ " " ++ show (S.toList cellVals) ++">"
> 
> type Board = M.Map Int Cell
> 
> emptyBoard :: Board
> emptyBoard = foldl' (\m i -> M.insert i (Cell i $ S.fromList [ONE .. NIN]) m) 
>                     M.empty [0 .. 80]

A `Digit` is just one of the nine possible values. It derives `Eq`, `Ord` and `Enum`. We use the
fact the `Digit` is enumerable to create a custom `Show` instance which is just the `Digit`'s ordinal
plus one so that `show ONE` gives "1".

A `Cell` has an index `cellIdx` and a set of possible digit values `cellVals`. The cell index is a 
number between 0 to 80 inclusive. The cell values denote the possible values the cell holds
without violating the rules of Sudoku. If a cell is filled, it holds only one value. We create a
custom `Show` instance of `Cell` to pretty print it.

A `Board` is just a map from the cell index to the corresponding cell for faster lookups than a
simple list of `Cell`s.

`emptyBoard` creates an empty board, with all the cells, unfilled by folding over all the
index list and inserting a cell with all possible digits in the map corresponding to each index.

Reading and Printing the Sudoku
-------------------------------

Next let's write some functions to read a Sudoku board from a string and to print a board so that we
can start playing with the actual examples. The board is represented as a single line with one 
digit for each cell if it is filled otherwise a dot `.`. The cells are read row first, left to 
right column. An example:

<pre>
6..3.2....4.....1..........7.26............543.........8.15........4.2........7..
</pre>

> readBoard :: String -> Maybe Board
> readBoard str = do
>   guard $ length str == 81
>   foldM (\board (i, chr) -> do
>             guard $ chr == '.' || (chr `S.member` S.fromList ['1' .. '9'])
>             let cellVals = if chr == '.'
>                              then S.fromList [ONE .. NIN]
>                              else S.singleton $ toEnum $ digitToInt chr - 1
>             return $ M.insert i (Cell i cellVals) board)
>         emptyBoard 
>         $ zip [0 .. 80 ] str

`readBoard` converts a string to a `Board`. It returns `Just Board` if the string represents a valid
Sudoku board, otherwise it returns `Nothing`. We use the `Monad` nature of `Maybe` to guard against the
possible failures. The guards fail if the string length is not exactly 81 or if it contains characters
other than 1 to 9 and `.`. The cells in the board returned have exactly one cell value if the 
string contained a digit at the cell index else they have all the digits as cell values.

> showBoard :: Board -> String
> showBoard =
>   map (\Cell{..} ->
>           if S.size cellVals == 1 
>             then intToDigit . (+ 1) . fromEnum . head . S.toList $ cellVals
>             else '.')
>   . M.elems
> 
> asciiShowBoard :: Board -> String
> asciiShowBoard =
>   (\t -> border ++ "\n" ++ t ++ border ++ "\n")
>   . unlines . intercalate [border] . chunksOf 3
>   . map ((\r -> "| " ++ r ++ " |")
>          . intercalate " | " . map (intersperse ' ') . chunksOf 3)
>   . chunksOf 9
>   . showBoard
>   where border = "+-------+-------+-------+"

`showBoard` does the reverse of `readBoard`. It takes a board and creates a valid string
representation of it. It does so by mapping over each cell of the board in the order of their index
and outputting the digit if the cell is filled else `.`.

`asciiShowBoard` converts a board to an ASCII graphic Sudoku board like we are used to see. It does
so by taking the output of `showBoard`, breaking it into chunks corresponding to rows and blocks, 
inserting spaces and `|` at appropriate places and then joining them with the borders made of `-`.

Here is an example run in ghci:

<pre>
*Sudoku> let boardStr = "6..3.2....4.....1..........7.26............543.........8.15........4.2........7.."
*Sudoku> let (Just board) = readBoard boardStr
*Sudoku> showBoard board 
"6..3.2....4.....1..........7.26............543.........8.15........4.2........7.."
*Sudoku> putStr (prettyShowBoard board)
+-------+-------+-------+
| 6 . . | 3 . 2 | . . . |
| . 4 . | . . . | . 1 . |
| . . . | . . . | . . . |
+-------+-------+-------+
| 7 . 2 | 6 . . | . . . |
| . . . | . . . | . 5 4 |
| 3 . . | . . . | . . . |
+-------+-------+-------+
| . 8 . | 1 5 . | . . . |
| . . . | . 4 . | 2 . . |
| . . . | . . . | 7 . . |
+-------+-------+-------+
</pre>

The post can be downloaded as a compilable Literate Haskell file [here][2]. The Haskell code in the
post can be downloaded [here][3].

[1]: http://hackage.haskell.org/package/split
[2]: /downloads/code/sudoku1.lhs
[3]: /downloads/code/sudoku1.hs
