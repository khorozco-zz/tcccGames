data Players = X
             | O
    deriving Show

type Square = Either Char Player

board :: String -> [[Players]] -> [[Players]]
board c [[E, E, E], [E, E, E], [E, E, E]] = boardUpdate
    where boardUpdate = if (c == "A1") then [[E, E, E], [E, E, E], [X, E, E]]
                                       else [[E, E, E], [E, E, E], [E, E, E]]

-- 3
-- 2
-- 1
--   A  B  C 
