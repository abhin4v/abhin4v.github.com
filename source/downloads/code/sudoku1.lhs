
---
layout: post
title: "Sudoku and Haskell: A Sudoku Solver"
date: 2013-03-09 23:35
comments: true
categories: programming haskell sudoku
published: false
---

Sudoku a popular number placement puzzle. It is played on a 9 by 9 grid where the objective is to
fill each cell in the grid with digits from 1 to 9 such that each column, each row and each 3 by 3
sub-grids (called blocks) have every digit from 1 to 9. The puzzle starts with some cells pre-filled 
and the player has to fill the rest to reach the solution. Since each unit (column, row or block) has
9 cells and has to be filled with all 9 digits - 1 to 9 - there cannot be any duplicates in a unit.

{% img https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Sudoku-by-L2G-20050714.svg/250px-Sudoku-by-L2G-20050714.svg.png 200 A typical Sudoku puzzle %}
{% img https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Sudoku-by-L2G-20050714_solution.svg/250px-Sudoku-by-L2G-20050714_solution.svg.png 200 The same puzzle with solution numbers marked in red %}

In this post we look at how to solve a Sudoku with Haskell. 

<!-- more -->

The code in this post has dependencies on the [`split`][1] package from Hackage.

Basic setup
-----------

> {-# LANGUAGE BangPatterns, RecordWildCards #-}
> 
> module Sudoku where
> 
> import qualified Data.Set as S
> import qualified Data.Map as M
> import Data.Char (digitToInt, intToDigit)
> import Data.List (foldl', intersperse, intercalate, sortBy, nub)
> import Data.List.Split (chunksOf)
> import Data.Maybe (fromJust)
> import Data.Ord (comparing)
> import Control.Monad (foldM, guard)
>

Now that the imports are out of the way let's setup the basic functionalities.
 
> data Digit = ONE | TWO | TRE | FOR | FIV | SIX | SVN | EGT | NIN
>              deriving (Eq, Ord, Enum)
> 
> instance Show Digit where
>   show digit = show $ fromEnum digit + 1
> 
> allDigits = S.fromList [ONE .. NIN]
> 
> data Cell = Cell { cellIdx :: Int, cellVals :: S.Set Digit }
>             deriving (Eq, Ord)
> 
> instance Show Cell where
>   show Cell{..} = "<" ++ show cellIdx ++ " " ++ show (S.toList cellVals) ++">"
> 
> newtype Board = Board (M.Map Int Cell)
>                 deriving (Eq, Ord)
> 
> boardCells :: Board -> [Cell]
> boardCells (Board ixMap) = M.elems ixMap
> 
> cellAt :: Board -> Int -> Maybe Cell
> cellAt (Board ixMap) idx = M.lookup idx ixMap
> 
> updateBoard :: Board -> Cell -> Board
> updateBoard (Board ixMap) cell@Cell{..} = Board (M.insert cellIdx cell ixMap)
> 
> emptyBoard :: Board
> emptyBoard = 
>   Board $ foldl' (\m i -> M.insert i (Cell i allDigits) m) M.empty [0 .. 80]
>

A `Digit` is just one of the nine possible values. It derives `Eq`, `Ord` and `Enum`. We use the
fact the `Digit` is enumerable to create a custom `Show` instance which is just the `Digit`'s ordinal
plus one so that `show ONE` gives "1".

A `Cell` has an index `cellIdx` and a set of possible digit values `cellVals`. The cell index is a 
number between 0 to 80 inclusive. The cell values denote the possible values the cell holds
without violating the rules of Sudoku. If a cell is filled, it holds only one value. We create a
custom `Show` instance of `Cell` to pretty print it.

A `Board` is just a wrapper over a map from the cell index to the corresponding cell. We use a map
instead of a simple list of `Cell`s for faster lookups.

`boardCells`, `cellAt` and `updateBoard` are some convenience functions to manipulate a board.
`boardCells` returns a list of all the cells in a board, `cellAt` returns a cell in a board at
a given index and `updateBoard` update a given cell in a board.

`emptyBoard` creates an empty board, with all the cells, unfilled by folding over all the
index list and inserting a cell with all possible digits in the map corresponding to each index.

Reading and printing the Sudoku
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
>                              then allDigits
>                              else S.singleton $ toEnum $ digitToInt chr - 1
>             return $ updateBoard board (Cell i cellVals))
>         emptyBoard 
>         $ zip [0 .. 80 ] str
>

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
>   . boardCells
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
> 
> instance Show Board where
>   show = showBoard
>

`showBoard` does the reverse of `readBoard`. It takes a board and creates a valid string
representation of it. It does so by mapping over each cell of the board in the order of their index
and outputting the digit if the cell is filled else `.`.

`asciiShowBoard` converts a board to an ASCII graphic Sudoku board like we are used to see. It does
so by taking the output of `showBoard`, breaking it into chunks corresponding to rows and blocks, 
inserting spaces and `|` at appropriate places and then joining them with the borders made of `-`.

Lastly, we add a `Show` instance of `Board` using `showBoard`.

Here is an example run in _ghci_:

<pre>haskell
*Sudoku> let boardStr = "6..3.2....4.....1..........7.26............543.........8.15........4.2........7.."
*Sudoku> let (Just board) = readBoard boardStr
*Sudoku> showBoard board 
"6..3.2....4.....1..........7.26............543.........8.15........4.2........7.."
*Sudoku> putStr (asciiShowBoard board)
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

Is it solved yet?
--------------------

Before we proceed to write a full-fledged Sudoku solver, it is good to have a function which tells us
whether a board is filled completely and whether that solution is a valid one.

> data BoardState = SOLVED | INCOMPLETE | INVALID
>                   deriving (Eq, Show)
> 
> boardState :: Board -> BoardState
> boardState board
>   | any (\Cell{..} -> S.size cellVals /= 1) $ boardCells board = INCOMPLETE
>   | any isUnitInvalid units = INVALID
>   | otherwise = SOLVED
>   where
>     isUnitInvalid unitCells = 
>       (S.fromList . map (head . S.toList . cellVals) $ unitCells) /= allDigits
> 
>     units = map (map (fromJust . cellAt board)) unitIxs
> 
> unitIxs   = rowIxs ++ columnIxs ++ blockIxs
> rowIxs    = map (\i -> [i * 9 .. i * 9 + 8]) [0..8]
> columnIxs = map (\i -> take 9 [i, i + 9 ..]) [0..8]
> blockIxs  = 
>   concatMap (\(row1:row2:row3:_) ->
>                 zipWith3 (\blockRow1 blockRow2 blockRow3 ->
>                              blockRow1 ++ blockRow2 ++ blockRow3) 
>                          row1 row2 row3)
>   . chunksOf 3 . map (chunksOf 3) $ rowIxs
>

We start by defining the board state as an enumeration of three value corresponding to the solved,
incomplete and invalid states. The `boardState` function takes a board and gives its current state.
It does so by checking three conditions:
 
1. if any cell in the board does not have only one possible value then the board is incomplete
2. if any unit of the board is invalid then the solution is invalid
3. else the board is solved

To find if an unit is invalid, we take all the cells of the unit and check if they have all the digits
in between them as per the rules of Sudoku.

Units are found just by looking up the indexes from the board for each unit. Unit indexes are all the
row, column and block indexes taken together. Row and column indexes can be obtained from the simple
mathematical formulas. Block indexes are a little trickier to get. It involves taking the row indexes,
splitting each row into chunks of three columns, then taking three rows at a time and mapping and 
concatenating them with a function which zips three rows at a time creating the block indexes.

A run in _ghci_ shows the indexes to be correct:

<pre>haskell
*Sudoku> mapM_ print rowIxs 
[0,1,2,3,4,5,6,7,8]
[9,10,11,12,13,14,15,16,17]
[18,19,20,21,22,23,24,25,26]
[27,28,29,30,31,32,33,34,35]
[36,37,38,39,40,41,42,43,44]
[45,46,47,48,49,50,51,52,53]
[54,55,56,57,58,59,60,61,62]
[63,64,65,66,67,68,69,70,71]
[72,73,74,75,76,77,78,79,80]

*Sudoku> mapM_ print columnIxs 
[0,9,18,27,36,45,54,63,72]
[1,10,19,28,37,46,55,64,73]
[2,11,20,29,38,47,56,65,74]
[3,12,21,30,39,48,57,66,75]
[4,13,22,31,40,49,58,67,76]
[5,14,23,32,41,50,59,68,77]
[6,15,24,33,42,51,60,69,78]
[7,16,25,34,43,52,61,70,79]
[8,17,26,35,44,53,62,71,80]

*Sudoku> mapM_ print blockIxs 
[0,1,2,9,10,11,18,19,20]
[3,4,5,12,13,14,21,22,23]
[6,7,8,15,16,17,24,25,26]
[27,28,29,36,37,38,45,46,47]
[30,31,32,39,40,41,48,49,50]
[33,34,35,42,43,44,51,52,53]
[54,55,56,63,64,65,72,73,74]
[57,58,59,66,67,68,75,76,77]
[60,61,62,69,70,71,78,79,80]
</pre>

See how the row indexes follow the grid indexes as we have taken our grid indexes to be rows first,
left to right. If we take row indexes column-wise we get the column indexes. If we take the row
indexes block-wise we get the block indexes.

Let's do a few sample runs of `boardState` in _ghci_:

<pre>haskell
*Sudoku> let (Just board) = readBoard "483921657967345821251876493548132976729564138136798245372689514814253769695417382"
*Sudoku> putStr (asciiShowBoard board)
+-------+-------+-------+
| 4 8 3 | 9 2 1 | 6 5 7 |
| 9 6 7 | 3 4 5 | 8 2 1 |
| 2 5 1 | 8 7 6 | 4 9 3 |
+-------+-------+-------+
| 5 4 8 | 1 3 2 | 9 7 6 |
| 7 2 9 | 5 6 4 | 1 3 8 |
| 1 3 6 | 7 9 8 | 2 4 5 |
+-------+-------+-------+
| 3 7 2 | 6 8 9 | 5 1 4 |
| 8 1 4 | 2 5 3 | 7 6 9 |
| 6 9 5 | 4 1 7 | 3 8 2 |
+-------+-------+-------+
*Sudoku> boardState board
SOLVED
*Sudoku> let (Just board) = readBoard "48392165796734582125187649354813297672956413.136798245372689514814253769695417382"
*Sudoku> boardState board
INCOMPLETE
*Sudoku> let (Just board) = readBoard "183921657967345821251876493548132976729564138136798245372689514814253769695417382"
*Sudoku> boardState board
INVALID
</pre>

That seems to be working. Now let's move on to actually solving the Sudoku!

Depth first search
-------------------

One way to solve Sudoku is to think of it as a graph search problem. Each board configuration becomes
a node in the search graph with the moves linking them as edges. A move is filling a particular cell
with a digit. So now we can solve the board just by finding a path from the given board configuration
to a configuration where all cells are filled.

We can use [Depth first Search][5] (DSF) algorithm to accomplish this. DFS is a brute force
technique and in worst case it may visit all the nodes in the search graph. In case of Sudoku, this
search graph is very large (approximately 6.67×10<sup>21</sup>) so this is not a very efficient way 
of solving Sudoku. For now, we'll add one optimization in DFS: while listing the next possible 
configurations for a particular configuration, we start with the cell with smallest number of cell 
values. This does not help us in the worst case but it will generally speed up things a little.
We can write this in two parts: a general DFS function and a solver which uses it to solve a Sudoku.

> dfs :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> [a]
> dfs start getNext isGoal = go start S.empty
>   where 
>     go node visited
>       | isGoal node = [node]
>       | S.member node visited = []
>       | otherwise = concatMap (\nextNode ->
>                                go nextNode (S.insert node visited))
>                               (getNext node)
> 
> dfsSolver :: Board -> [Board]
> dfsSolver board = dfs board nextBoards ((== SOLVED) . boardState)
>   where
>     nextBoards board = 
>       map (updateBoard board) 
>       . concatMap (\Cell{..} -> map (Cell cellIdx . S.singleton) . S.toList $ cellVals)
>       . sortBy (comparing (S.size . cellVals)) 
>       . filter ((/= 1) . S.size . cellVals) 
>       . boardCells
>       $ board
> 

Let's try this out in _ghci_ now:

<pre>haskell
*Sudoku> let boardStr = ".839216579.734582125187649354813297672956413813679824537268951481425376969541738."
*Sudoku> let (Just board) = readBoard boardStr
*Sudoku> :set +s
*Sudoku> (mapM_ print . nub . dfsSolver) board
483921657967345821251876493548132976729564138136798245372689514814253769695417382
(1.75 secs, 356010984 bytes)
</pre>

<pre>haskell
*Sudoku> let boardStr = ".839216579.734582125187.49354813297672956413813679824537268951481425376969541738."
*Sudoku> let (Just board) = readBoard boardStr
*Sudoku> :set +s
*Sudoku> (mapM_ print . nub . dfsSolver) board
483921657967345821251876493548132976729564138136798245372689514814253769695417382
(66.64 secs, 13136374896 bytes)
*Sudoku> (print . head . dfsSolver) board
483921657967345821251876493548132976729564138136798245372689514814253769695417382
(7.32 secs, 1322834576 bytes)
</pre>

Constraint propagation
----------------------

The finale
----------

What's next
-----------

Get the code
------------

This post can be downloaded as a compilable Literate Haskell file [here][2]. The Haskell code in the
post can be downloaded [here][3] or can be forked [here][4].

[1]: http://hackage.haskell.org/package/split
[2]: /downloads/code/sudoku1.lhs
[3]: /downloads/code/sudoku1.hs
[4]: https://github.com/abhin4v/abhin4v.github.com/blob/source/source/downloads/code/sudoku1.hs
[5]: http://en.wikipedia.org/wiki/Depth_first_search
