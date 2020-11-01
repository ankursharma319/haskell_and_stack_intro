module PhillipTutorial where

import Data.List

prefixes :: [a] -> [[a]]
prefixes = foldr prefixesHelper [[]]

prefixesHelper :: b -> [[b]] -> [[b]]
prefixesHelper y x = [y] : map ((:) y) x


data Trie a = Leaf a | Node a [Trie a]

foldTrie :: (b -> a -> b) -> b -> Trie a -> b
foldTrie f acc (Leaf x) = f acc x
foldTrie f acc (Node x xs) = foldl (foldTrie f) (f acc x) xs


