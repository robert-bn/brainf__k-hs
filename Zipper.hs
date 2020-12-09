module Zipper where
import Control.Monad

data Zipper a = Zipper [a] a [a]
    deriving (Show, Eq)

-- Start zipper from left of list
startZipper :: [a] -> Zipper a
startZipper (x:xs) = Zipper [] x xs
 
-- Moves zippers left/right, no checking if can move for
-- memory tape since we can use infinite list for that
unsafeLeft, unsafeRight :: Zipper a -> Zipper a
unsafeRight (Zipper ls p (r:rs)) = Zipper (p:ls) r rs 
unsafeLeft  (Zipper (l:ls) p rs) = Zipper ls l (p:rs)

inplace :: (a -> a) ->  Zipper a -> Zipper a
inplace f (Zipper ls x rs) = Zipper ls (f x) rs

replace :: a -> Zipper a -> Zipper a
replace x (Zipper ls _ rs) = Zipper ls x rs

current :: Zipper a -> a
current (Zipper _ x _) = x
