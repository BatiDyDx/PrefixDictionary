{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dict (Dic (..)) where

class Dic k v d | d -> k v where
  empty  :: d
  insert :: Ord k => k -> v -> d -> d
  search :: Ord k => k -> d -> Maybe v
  remove :: Ord k => k -> d -> d
  keys   :: Ord k => d -> [k]
