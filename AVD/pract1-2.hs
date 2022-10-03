import Test.QuickCheck

miInsert x [] = [x]

miInsert x (y:ys) | x < y = x : y : ys | otherwise = y : (miInsert x ys)

miOrdered [] = True

miOrdered [x] = True

miOrdered (x:y:ys) = x <= y && miOrdered (y:ys)

----------------------------------------------------------

prop_Ordered :: Int -> [Int] -> Property
prop_Ordered x (y:ys) = miInsert x (y:ys)
