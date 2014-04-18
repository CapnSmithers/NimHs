-- Dumb implementation of Nim
-- Computer will select the first row with sticks, and take all sticks in the row

module DumbNim where

    -- TODO: Sort out human move, it's allowing you to take from an empty row
    -- Also not allowing you to take valid moves in some cases

    data Player = Human | Computer
        deriving (Eq, Show, Bounded, Enum)
    type Row = Int
    type Board = [Row]
    type Move = (Row, Int)

    displayBoard :: Board -> String
    displayBoard bd = 
        "Row 1: " ++ (concat $ replicate (bd !! 0) "|") ++ "\n" ++ 
        "Row 2: " ++ (concat $ replicate (bd !! 1) "|") ++ "\n" ++
        "Row 3: " ++ (concat $ replicate (bd !! 2) "|") ++ "\n"

    -- Validates human move and passes to makeMove
    humanMove :: Board -> Move -> (Bool, Board)
    humanMove bd mv@(row, sticks)
        | valid = (True, makeMove bd (row-1, sticks))
        | otherwise = (False, bd)
        where validRow = (row >= 1 && row <= 3) && (bd !! (row-1)) /= 0
              validSticks = validRow && sticks > 0 && sticks <= (bd !! (row-1))
              valid = validRow && validSticks


    compMove :: Board -> Board
    compMove bd = bd -- sticks all sticks from first available row

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
        | empty && (player == Human) = "Computer"
        | empty && (player == Computer) = "Human"
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

                        if (winner board Computer /= "")
                            then do putStrLn ("Winner is: " ++ show (winner board Computer))
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