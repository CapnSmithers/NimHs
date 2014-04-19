-- Dumb implementation of Nim
-- Computer will select the first row with sticks, and take random sticks from the row

module RandomNim where

    import System.Random


    data Player = Human | Computer
        deriving (Eq, Show, Bounded, Enum)
    type Row = Int
    type Board = [Row]
    type Move = (Row, Int)

    displayBoard :: Board -> String
    displayBoard bd = 
        "Row 1: " ++ (concat $ replicate (bd !! 0) "|") ++ " \n" ++ 
        "Row 2: " ++ (concat $ replicate (bd !! 1) "|") ++ " \n" ++
        "Row 3: " ++ (concat $ replicate (bd !! 2) "|") ++ " \n"

    -- Validates human move and passes to makeMove
    humanMove :: Board -> Move -> (Bool, Board)
    humanMove bd mv@(row, sticks)
        | valid = (True, makeMove bd (row-1, sticks))
        | otherwise = (False, bd)
        where validRow = (row >= 1 && row <= 3) && (bd !! (row-1)) /= 0
              validSticks = validRow && sticks > 0 && sticks <= (bd !! (row-1))
              valid = validRow && validSticks

    -- Choose a random number of sticks to take from first available row
    compMove :: Board -> Board
    compMove bd
        | ((bd !! 0) > 0) = [(bd!!0) - firstRowSticks, (bd !! 1), (bd !! 2)]
        | ((bd !! 1) > 0) = [(bd!!0), (bd!!1)-secondRowSticks, (bd!!2)]
        | ((bd !! 2) > 0) = [(bd!!0), (bd!!1), (bd!!2)-thirdRowSticks]
        | otherwise = bd
        where 
            (firstRowSticks, firstGen) = (randomR (1, bd !! 0) (mkStdGen 5001))  :: (Int, StdGen)
            (secondRowSticks, secGen) = (randomR (1, bd !! 1) (mkStdGen 5002))  :: (Int, StdGen)
            (thirdRowSticks, thirdGen) = (randomR (1, bd !! 2) (mkStdGen 5003))  :: (Int, StdGen)
  

    ---Was going to use to choose a random row, but base case is funky
    chooseRandomRow :: (Num a, Ord a) => [a] -> Int
    chooseRandomRow bd@(x:xs)
        | bd == [] = 0
        | ((bd !! myRow) > 0) = myRow
        | otherwise = chooseRandomRow [x | x <- bd, x > 0]
        where
            (myRow, rowGen) = (randomR (0, length bd) (mkStdGen 32340324)) :: (Int, StdGen)

    -- Similar to the move fn in computer tic-tac-toe.  Recursively applies
    -- move to board.  More Haskell-like than using case
    makeMove :: Board -> Move -> Board
    makeMove (bdVal:bd) (row, sticks) 
        | row > 0 = bdVal:[] ++ makeMove bd ((row - 1), sticks)
        | otherwise = (bdVal - sticks):[] ++ bd

    winner :: Board -> Player -> String
    winner board player 
        -- Logic to determine winner
        | not empty = ""
        | empty && (player == Human) = "Human"
        | empty && (player == Computer) = "Computer"
        | otherwise = ""
        where
            empty = (length $ filter (\x -> x /= 0) board) == 0
            
    play :: Board -> IO ()
    play board = do
        if ((winner board Human) /= "")
            then do 
                putStrLn ("Winner is: " ++ show (winner board Human))
            else do
                putStrLn $ displayBoard board
                -- Play game move'                
                -- Human move
                putStr "Enter a row (1-3): "
                row <- getLine
                putStr ("Enter a number of sticks: ")
                sticks <- getLine

                let mv = (read (row), read (sticks))
                putStrLn $ show mv
                let (valid, hBoard) = humanMove board mv
                if (valid)
                    then do
                        putStrLn $ show hBoard

                        if (winner hBoard Computer /= "")
                            then do putStrLn ("Winner is: " ++ show (winner hBoard Computer))
                            else do
                                putStrLn $ displayBoard hBoard
                                let cBoard = compMove hBoard
                                play cBoard
                    else do
                        putStrLn "\nNot a possible move!\n"
                        play board

    main = do
        let board = [4,3,7]
        putStrLn "Let's play!\n"
        play board