type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
                 (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c `elem` [1..8] && `elem` [1..8])
    return (c', r')

nonMonadMoveKinght :: KnightPos -> [KnightPos]
nonMonadMoveKinght (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
     (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end -> end `elem` in3 start

-- monad laws
-- 1. Left Identity
--  // monad context shouldn't affect application of function
--     return x >>= f is the same as f x
--
-- 2. Right Identity
--  // if we have a monadic value and feed it to return, we get the monadic value back
--      m >>= return
--
--  // both rule 1 and 2 are about making normal values into monadic ones
--
-- 3. Associative
--      (m >>= f) >>= g  should equal m >>= (\x -> f x >>= g)
