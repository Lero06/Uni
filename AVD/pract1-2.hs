import Test.QuickCheck

miInsert x [] = [x]

miInsert x (y:ys) | x < y = x : y : ys | otherwise = y : (miInsert x ys)

miOrdered [] = True

miOrdered [x] = True

miOrdered (x:y:ys) = x <= y && miOrdered (y:ys)

----------------------------------------------------------

prop_Ordered :: Int -> [Int] -> Property
prop_Ordered x ys = miOrdered ys == True ==> miOrdered (miInsert x (ys)) == True


-- Ejercicio 9
prop_Ordered2 :: Int -> [Int] -> Property
prop_Ordered2 x ys = miOrdered ys == True ==> classify (null ys) "trivial" $ miOrdered (miInsert x (ys)) == True

-- Ejercicio 10
prop_Ordered3 :: Int -> [Int] -> Property
prop_Ordered3 x ys = miOrdered ys == True ==> collect (length ys) $ miOrdered (miInsert x (ys)) == True
