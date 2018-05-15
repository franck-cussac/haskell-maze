module Lib where

import System.Random
import Data.List

data Cell = Cell {
    isFree :: Bool,
    order :: [Int]
}

someFunc :: IO ()
someFunc = do
    g1 <- newStdGen
    g2 <- newStdGen
    g3 <- newStdGen
    g4 <- newStdGen
    g5 <- newStdGen
    putStrLn (toStringMaze (createWall g1 10 15 down (createWall g2 10 15 left (createWall g3 10 15 up (createWall g4 10 15 right (createEmptyMaze g5 20 40))))))
--someFunc = putStrLn (toStringMaze (replaceInGrid 5 10 (createEmptyMaze 20 30)))

boolToChar :: Bool -> Char
boolToChar True = ' '
boolToChar False = '#'

toFlat2 :: [Cell] -> [Char]
toFlat2 tab = map boolToChar (map isFree tab)

toFlat :: [[Cell]] -> [String]
toFlat maze = map toFlat2 maze

toStringMaze :: [[Cell]] -> String
toStringMaze maze = intercalate "\n" (toFlat maze)

left = 0
down = 1
up = 2
right = 3

shuffledList :: Int -> [Int]
shuffledList 0 = [up, right, down, left]
shuffledList 1 = [up, right, left, down]
shuffledList 2 = [up, down, right, left]
shuffledList 3 = [up, down, left, right]
shuffledList 4 = [up, left, down, right]
shuffledList 5 = [up, left, right, down]
shuffledList 6 = [right, down, left, up]
shuffledList 7 = [right, down, up, left]
shuffledList 8 = [right, left, down, up]
shuffledList 9 = [right, left, up, down]
shuffledList 10 = [right, up, down, left]
shuffledList 11 = [right, up, left, down]
shuffledList 12 = [down, left, right, up]
shuffledList 13 = [down, left, up, right]
shuffledList 14 = [down, right, left, up]
shuffledList 15 = [down, right, up, left]
shuffledList 16 = [down, up, left, right]
shuffledList 17 = [down, up, right, left]
shuffledList 18 = [left, right, down, up]
shuffledList 19 = [left, right, up, down]
shuffledList 20 = [left, up, down, right]
shuffledList 21 = [left, up, right, down]
shuffledList 22 = [left, down, right, up]
shuffledList 23 = [left, down, up, right]

createEmptyCellSeq :: Int -> [Cell]
createEmptyCellSeq i = Cell {
    isFree = True,
    order = shuffledList (i)
} : createEmptyCellSeq i


createFullCell :: Int -> Cell
createFullCell i = Cell {
    isFree = False,
    order = shuffledList (i)
}

createFullCellSeq :: Int -> [Cell]
createFullCellSeq i = Cell {
    isFree = False,
    order = shuffledList (i)
} : createFullCellSeq i


getRandom :: (RandomGen g) => g -> Int
getRandom g = i
    where (i, gen2) = (randomR (0, 23) g)

replace :: (RandomGen g) => g -> Int -> [Cell] -> [Cell]
replace g x row = if length row <= x
                then row
                else take x row ++ [createFullCell (getRandom g)] ++ (drop (x+1) row)

replaceInGrid :: (RandomGen g) => g -> Int -> Int -> [[Cell]] -> [[Cell]]
replaceInGrid g x y maze = take x maze ++ [replace g y (maze!!x)] ++ drop (x+1) maze

createRowSeq :: (RandomGen g) => g -> Int -> [[Cell]]
createRowSeq g n = replace g (n-1) (replace g 0 (take n (createEmptyCellSeq (getRandom g)))) : createRowSeq g n


createEmptyMaze :: (RandomGen g) => g -> Int -> Int -> [[Cell]]
createEmptyMaze g x y = take y (createFullCellSeq (getRandom g)) : take (x-2) (createRowSeq g y) ++ [take y (createFullCellSeq (getRandom g))]
--createEmptyMaze x y = [createRowSeq x False] ++ take (y-2) (createRowSeq x True) ++ createRowSeq x False

isBuildableCell :: Int -> Int -> Int -> [[Cell]] -> Bool
isBuildableCell x y 0 maze = isFree (maze!!x!!y) && isFree (maze!!(x-1)!!y) && isFree (maze!!(x-1)!!(y-1)) && isFree (maze!!(x+1)!!(y-1)) && isFree (maze!!x!!(y-1)) && isFree (maze!!(x+1)!!y)
isBuildableCell x y 1 maze = isFree (maze!!x!!y) && isFree (maze!!x!!(y-1)) && isFree (maze!!x!!(y+1)) && isFree (maze!!(x+1)!!(y-1)) && isFree (maze!!(x+1)!!(y+1)) && isFree (maze!!(x+1)!!y)
isBuildableCell x y 2 maze = isFree (maze!!x!!y) && isFree (maze!!x!!(y-1)) && isFree (maze!!(x-1)!!(y-1)) && isFree (maze!!(x-1)!!y) && isFree (maze!!(x-1)!!(y+1)) && isFree (maze!!x!!(y+1))
isBuildableCell x y 3 maze = isFree (maze!!x!!y) && isFree (maze!!(x-1)!!y) && isFree (maze!!(x-1)!!(y+1)) && isFree (maze!!x!!(y+1)) && isFree (maze!!(x+1)!!(y+1)) && isFree (maze!!(x+1)!!y)

createWall :: (RandomGen g) => g -> Int -> Int -> Int -> [[Cell]] -> [[Cell]]
createWall g x y 0 maze = do
                            let y2 = y - 1
                            let x2 = x
                            if (isBuildableCell x2 y2 0 maze)
                            then do
                                let newMaze = replaceInGrid g x2 y2 maze
                                createWall g1 x2 y2 ((order (newMaze!!x2!!y2))!!0) (
                                        createWall g2 x2 y2 ((order (newMaze!!x2!!y2))!!1) (
                                            createWall g3 x2 y2 ((order (newMaze!!x2!!y2))!!2) (
                                                createWall g4 x2 y2 ((order (newMaze!!x2!!y2))!!3) newMaze
                                            )
                                        )
                                    )
                            else maze
                                where
                                    (g1, g5) = split g
                                    (g2, g6) = split g5
                                    (g3, g4) = split g6
createWall g x y 1 maze = do
                            let y2 = y
                            let x2 = x + 1
                            if (isBuildableCell x2 y2 1 maze)
                            then do
                                let newMaze = replaceInGrid g x2 y2 maze
                                createWall g1 x2 y2 ((order (newMaze!!x2!!y2))!!0) (
                                        createWall g2 x2 y2 ((order (newMaze!!x2!!y2))!!1) (
                                            createWall g3 x2 y2 ((order (newMaze!!x2!!y2))!!2) (
                                                createWall g4 x2 y2 ((order (newMaze!!x2!!y2))!!3) newMaze
                                            )
                                        )
                                    )
                            else maze
                                where
                                    (g1, g5) = split g
                                    (g2, g6) = split g5
                                    (g3, g4) = split g6
createWall g x y 2 maze = do
                            let y2 = y
                            let x2 = x - 1
                            if (isBuildableCell x2 y2 2 maze)
                            then do
                                let newMaze = replaceInGrid g x2 y2 maze
                                createWall g1 x2 y2 ((order (newMaze!!x2!!y2))!!0) (
                                        createWall g2 x2 y2 ((order (newMaze!!x2!!y2))!!1) (
                                            createWall g3 x2 y2 ((order (newMaze!!x2!!y2))!!2) (
                                                createWall g4 x2 y2 ((order (newMaze!!x2!!y2))!!3) newMaze
                                            )
                                        )
                                    )
                            else maze
                                where
                                    (g1, g5) = split g
                                    (g2, g6) = split g5
                                    (g3, g4) = split g6
createWall g x y 3 maze = do
                            let y2 = y + 1
                            let x2 = x
                            if (isBuildableCell x2 y2 3 maze)
                            then do
                                let newMaze = replaceInGrid g x2 y2 maze
                                createWall g1 x2 y2 ((order (newMaze!!x2!!y2))!!0) (
                                        createWall g2 x2 y2 ((order (newMaze!!x2!!y2))!!1) (
                                            createWall g3 x2 y2 ((order (newMaze!!x2!!y2))!!2) (
                                                createWall g4 x2 y2 ((order (newMaze!!x2!!y2))!!3) newMaze
                                            )
                                        )
                                    )
                            else maze
                                where
                                    (g1, g5) = split g
                                    (g2, g6) = split g5
                                    (g3, g4) = split g6