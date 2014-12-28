module Maximum where

run :: (Ord a) => [a] -> a
run [] = error "hello"
run [x] = x
run (x:xs)
   | x > maxTail  =  x
   | otherwise    = maxTail
   where maxTail = run(xs)

run2 :: (Ord a) => [a] -> a
run2 [] = error "error"
run2 [x] = x
run2 (x:xs) = max x (run2 xs)
