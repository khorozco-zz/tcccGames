
data Players = X
             | O
    deriving Show

type Square = Either Char Players

initialBoard :: (String a) => a
initialBoard = undefined

--   +---+---+---+
-- 3 |   |   |   |
--   +---+---+---+
-- 2 |   |   |   |
--   +---+---+---+
-- 1 |   |   |   |
--   +---+---+---+
--     A   B   C 