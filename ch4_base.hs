import Distribution.Simple.Utils (xargs)
--null'
null' :: [a] -> Bool
null' xs = if length xs == 0 then True else False

--null' cond



--null'' pattern
null'' :: [a] -> Bool
null'' [] = True
null'' _  = False

--null''' guarded
null''' xs 
   | xs == [] = True
   | otherwise = False


--safetail
--safetail cond
safetail :: [a] -> [a]
safetail xs =  if null xs then [] else tail xs


--safetail pattern
safetail' :: [a] -> [a]
safetail' [] = []
safetail' xs = tail xs


--safetail guarded
safetail'' :: [a] -> [a]
safetail'' xs
   | null xs = []
   | otherwise = tail xs


--explicit
{-
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True  = True
True  ||| False = True
True  ||| True  = True
-}

--match both arguments
(|||) :: Bool -> Bool -> Bool
False ||| False = False
_     ||| _     = True

--match first arg only
(||||) :: Bool -> Bool -> Bool
False |||| x = x
_     |||| _ = True


-- && with cond
{- True && True = True
   _    && _    = False -}
--(&&) :: Bool -> Bool -> Bool
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then if y then True else False else False


--above, again, with slightly different logic
(&&&&) :: Bool -> Bool -> Bool
x &&&& y = if x then y else False



