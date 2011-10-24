module Trie where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data (Ord k) => Trie k a
  = Leaf a
  | Node (Map k (Trie k a))
  deriving (Read, Show)

concat :: (Eq a, Ord k, Show k, Show a) =>
          [Trie k a] -> Trie k a
concat = fromList . (>>= toList)

fold :: (Eq a, Ord k, Show k, Show a) =>
        (([k], a) -> b -> b) -> b -> Trie k a -> b
fold s z = foldr s z . toList

fromList :: (Eq a, Ord k, Show k, Show a) =>
            [([k], a)] -> Trie k a
fromList = foldr add empty

toList :: (Ord k) => Trie k a -> [([k], a)]
toList t = map (first reverse) $ aux [] t []
  where
    aux ks (Leaf a) acc = (ks, a) : acc
    aux ks (Node m) acc = Map.foldrWithKey (aux . (: ks)) acc m

empty :: (Ord k) => Trie k a
empty = Node Map.empty

add :: (Eq a, Ord k, Show k, Show a) =>
       ([k], a) -> Trie k a -> Trie k a
add ksa t =
  case add' ksa t of
    Left (ksa1, ksa2) -> error $ Prelude.concat
      [ "Trie.add: adding ", show ksa1
      , " to ", show ksa2, "." ]
    Right nt -> nt

add' :: (Eq a, Ord k) => ([k], a) -> Trie k a
     -> Either (([k], a), ([k], a)) (Trie k a)
add' ksa@(ks, a1) t@(Leaf a2)
  | null ks && a1 == a2 = Right t
  | otherwise = Left (ksa, ([], a2))
add' ksa@([], _) t@(Node _) =
  Left (ksa, fromJust $ witness t)
  -- TODO: this code is dangerous
add' (k:ks, a) (Node m) =
  case Map.lookup k m of
    Just t ->
      case add' (ks, a) t of
        Left ((ks1, a1), (ks2, a2)) ->
          Left ((k:ks1, a1), (k:ks2, a2))
        Right nt -> insert nt
    Nothing -> insert $ singleton (ks, a)
  where insert t = Right $ Node $ Map.insert k t m

-- TODO: this code is false
--   we have to avoid the node with empty maps
witness :: (Ord k) => Trie k a -> Maybe ([k], a)
witness (Leaf a) = Just ([], a)
witness (Node m) = do
  (k, t) <- listToMaybe $ Map.toList m
  (ks, a) <- witness t
  return (k:ks, a)

singleton :: (Ord k) => ([k], a) -> Trie k a
singleton ([], a) = Leaf a
singleton (k:ks, a) = Node $ Map.singleton k $ singleton (ks, a)

