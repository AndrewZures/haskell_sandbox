doubleMe x = x*2
doubleUs x y = x*2 + y*2

doubleSmall x = if x > 100
                  then x
                  else x*2

length' xs = sum [1 | _ <- xs]

removeUpperCase st = [c | c <- st, c `elem` ['a'..'z']]

evenNest xxs = [[x | x <- xs, even x] | xs <- xxs ]

