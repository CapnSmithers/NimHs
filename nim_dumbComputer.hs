-- Dumb implementation of Nim
-- Computer will select the first row with sticks, and take all sticks in the row

module DumbNim where

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

    humanMove :: Board -> Board
    humanMove


    compMove :: Board -> Board
    compMove

    winner :: Board -> Player -> String
    winner board player = 
        -- Logic to determine winner
        | not empty = ""
        | empty && player == Human = "Computer"
        | empty && player == Computer = "Human"
        | otherwise = ""
        where
            empty = (length $ filter (\x -> x /= 0) board) == 0
            
    play :: Board -> Player -> Io ()
    play board player = do
        if ((winner board player) /= "")
            then do 
                putStrLn ("Winner is: " ++ show (winner board player))
            else do
                -- Play game move'
                if (player == Human)
                    then do 
                        -- Human move
                        let newBoard = humanMove board
                        play newBoard Computer
                    else do
                        -- Computer move
                        let newBoard = compMove board
                        play newBoard Human


    main = do
        board = [4,3,7]
        putStrLn "Let's play!"
        play board Human