import Test.QuickCheck
-- Ejercicio 1

-- prop_RevUnit :: Int -> Int
-- prop_RevUnit x = reverse (x) == x

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs++ys) == reverse ys++reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y
