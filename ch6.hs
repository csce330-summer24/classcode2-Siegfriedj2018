and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' [] = True
and'' (False:_) = False
and'' (True:bs) = and'' bs

and''' :: Foldable t => t Bool -> Bool
and''' xs = foldr (&&) True xs

and'''' :: [Bool] -> Bool
and'''' = foldr (&&) True

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

concat'' :: Foldable t => t [a] -> [a]
concat'' xs = foldr (++) [] xs 

replicate' :: (Eq t1, Num t1) => t1 -> t2 -> [t2]
replicate' 0 _ = []
replicate' n c = c : replicate' (n-1) c

repeat' :: t -> [t]
repeat' c = c: repeat' c

replicate'' :: Int -> a -> [a]
replicate'' n c = take n (repeat' c)

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y    = True
  | otherwise = elem' x ys

elem'' :: Eq t => t -> [t] -> Bool
elem'' _ [] = False
elem'' x (y:ys) = if x==y then True else elem'' x ys


merge :: Ord a => [a] -> [a] ->[a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    half = length xs `div` 2
    (left,right) = splitAt half xs