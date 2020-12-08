module Zipper where
import Control.Monad

data Zipper a = Zipper [a] a [a]
    deriving (Show, Eq)

instance Functor Zipper where
    fmap f (Zipper ls p rs) = Zipper (fmap f ls) (f p) (fmap f rs)

instance Foldable Zipper where
    foldMap f (Zipper ls x rs) = foldMap f (ls ++ x:rs)
    foldr f i (Zipper ls x rs) = foldr f i (ls ++ x:rs)

instance Traversable Zipper where
    -- sequenceA :: Applicative f => Zipper (f a) -> f (Zipper a)
    sequenceA (Zipper ls x rs) = Zipper <$> sequenceA ls <*> x <*> sequenceA rs

-- Start zipper from left of list
startZipper :: [a] -> Zipper a
startZipper (x:xs) = Zipper [] x xs
 
 -- Check if zipper can be moved left/right
canMoveRight, canMoveLeft :: Zipper a -> Bool
canMoveRight (Zipper _ _ [])  = False
canMoveRight _                = True
canMoveLeft (Zipper [] _ _) = False
canMoveLeft _               = True

-- Moves zippers left/right, no checking if can move for
-- memory tape since we can use infinite list for that
unsafeLeft, unsafeRight :: Zipper a -> Zipper a
unsafeRight (Zipper ls p (r:rs)) = Zipper (p:ls) r rs 
unsafeLeft  (Zipper (l:ls) p rs) = Zipper ls l (p:rs)
                                  
zipLeft, zipRight :: Int -> Zipper a -> Zipper a
zipLeft  n z = iterate unsafeLeft  z !! n
zipRight n z = iterate unsafeRight z !! n
                                 

-- moves zipper one to the left/right
left, right :: Zipper a -> Maybe (Zipper a)
left z = unsafeLeft z <$ guard (canMoveLeft z)
right z = unsafeRight z <$ guard (canMoveRight z)

inplace :: (a -> a) ->  Zipper a -> Zipper a
inplace f (Zipper ls x rs) = Zipper ls (f x) rs

replace :: a -> Zipper a -> Zipper a
replace x (Zipper ls _ rs) = Zipper ls x rs

current :: Zipper a -> a
current (Zipper _ x _) = x
