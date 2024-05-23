module PrefixTree where

import Dict (Dic (..))
import Data.Maybe (isJust)

data PTree k v  = Node k (Maybe v) (PTree k v) (PTree k v) (PTree k v)
                | Leaf k v
                | Empty
                deriving Show

searchPT :: Ord k => [k] -> PTree k v -> Maybe v
searchPT _ Empty = Nothing
searchPT [] _    = Nothing
searchPT (x:xs) (Leaf y v)  | x == y = case xs of
                                        [] -> Just v
                                        _  -> Nothing
                            | otherwise = Nothing
searchPT s@(x:xs) (Node y v l m r)
          | x < y = searchPT s l
          | x > y = searchPT s r
          | otherwise =
            case xs of
              [] -> v
              _  -> searchPT xs m

-- insertInEmpty xs v = insertPT xs v Empty siempre y cuando xs != []
insertInEmpty :: [k] -> v -> PTree k v
insertInEmpty [x] v = Leaf x v
insertInEmpty (x:xs) v = Node x Nothing Empty (insertInEmpty xs v) Empty

insertPT :: Ord k => [k] -> v -> PTree k v -> PTree k v
insertPT [] _ _ = Empty
insertPT xs v Empty = insertInEmpty xs v
insertPT (x:xs) v t@(Leaf y w)
            | x < y = case xs of
                        [] -> Node x (Just v) Empty Empty t
                        _  -> let m = insertInEmpty xs v
                              in Node x Nothing Empty m t
            | x > y = case xs of
                        [] -> Node x (Just v) t Empty Empty
                        _  -> let m = insertInEmpty xs v
                              in Node x Nothing Empty m t
            | otherwise = case xs of
                            [] -> Leaf x v
                            _  -> let m = insertInEmpty xs v
                                  in Node x (Just w) Empty m Empty
insertPT s@(x:xs) v (Node y w l m r)
            | x < y = let l' = insertPT s v l
                      in Node y w l' m r
            | x > y = let r' = insertPT s v r
                      in Node y w l m r'
            | otherwise = case xs of
                            [] -> Node x (Just v) l m r
                            _  -> let m' = insertPT xs v m
                                  in Node x w l m' r


removePT :: Ord k => [k] -> PTree k v -> PTree k v
removePT [] t = t
removePT _ Empty = Empty
removePT [x] t@(Leaf y _) | x == y = Empty
                        | otherwise = t
removePT (_:_) t@(Leaf _ _) = t
removePT s@(x:xs) (Node y v l m r)
      | x < y = let l' = removePT s l
                in prune y v l' m r
      | x > y = let r' = removePT s r
                in prune y v l m r'
      | otherwise = case xs of
                      [] -> prune y Nothing l m r
                      _  -> let m' = removePT xs m
                            in prune y v l m' r
      where
        prune x Nothing  Empty Empty Empty = Empty
        prune x (Just v) Empty Empty Empty = Leaf x v
        prune x v l m r = Node x v l m r

keysPT :: PTree k v -> [[k]]
keysPT Empty = []
keysPT (Leaf k v) = [[k]]
keysPT (Node x v l m r) = keysPT l ++ subkeys ++ keysPT r
  where
    self = if isJust v then [[x]] else []
    subkeys = self ++ map (x:) (keysPT m)


instance Ord k => Dic [k] v (PTree k v) where
  empty  = Empty
  search = searchPT
  insert = insertPT
  remove = removePT
  keys   = keysPT

t :: PTree Char Int
t = Node 'r' Nothing
        Empty
        (Node 'e' (Just 16)
            (Node 'a' Nothing
                Empty
                (Leaf 's' 1)
                Empty)
            (Node 'o' (Just 2)
                (Leaf 'd' 9)
                Empty
                (Leaf 's' 4))
            Empty)
        (Node 's' Nothing
            Empty
            (Node 'i' (Just 4)
                (Leaf 'e' 8)
                (Leaf 'n' 7)
                Empty)
            Empty)
