add :: Num a => (a, a) -> a
add (x,y) = x+y

add' :: Num a => a -> a -> a
add' x y = x+y

second :: [a] -> a
second xs = head (tail xs)

-- !! is another way to index a list
second' :: [a] -> a
second' = (!!1)

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x) 

