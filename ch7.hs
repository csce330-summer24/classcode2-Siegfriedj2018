filtmap :: (t -> Bool) -> (t -> a) -> [t] -> [a]
filtmap p f xs = [f x | x <- xs, p x]

filtmap' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap' p f xs = map f (filter p xs)

filtmap'' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap'' p f = (map f).(filter p)

map' :: (t -> a) -> [t] -> [a]
map' f xs = [f x | x<-xs]

map'' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map'' f xs = foldr (\x rest-> f x : rest) [] xs 

map''' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map''' f xs = foldr (\x rest-> [f x] ++ rest) [] xs

map'''' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map'''' f xs = foldl (\prev x->prev++[f x]) [] xs

filter' p xs = [x | x<-xs, p x]

filter'' :: Foldable t => (a -> Bool) -> t a -> [a]
filter'' p xs = foldr (\x rest -> if p x then x:rest else rest) [] xs

max' :: Ord a => [a] -> a
max' (x:xs) = foldr (\y max_rest->if y>max_rest then y else max_rest) x xs