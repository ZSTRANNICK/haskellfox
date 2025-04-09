module Utils
    ( isEmpty
    , unwrapFilterJust
    , limitOrd
    ) where

-- [FUNCTIONS] --

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

unwrapFilterJust :: [Maybe a] -> [a]
unwrapFilterJust [] = []
unwrapFilterJust (Just x : xs) = x : unwrapFilterJust xs
unwrapFilterJust (_ : xs) = unwrapFilterJust xs

limitOrd :: (Ord a) => a -> a -> a -> a
limitOrd l r v | v < l = l
               | v > r = r
               | otherwise = v
