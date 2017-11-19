-- | A list-like data structure with fast prepend and cache-friendly fold. It
-- supports both boxed and unboxed elements.
module Data.Prependotron
  ( -- * Data type
    Prependotron

    -- * Creating prependotrons
  , empty
  , fromFoldable

    -- * Modifying prependotrons
  , cons

    -- * Folding prependotrons
  , foldl'
  ) where

import Control.Parallel (par)
import Data.Foldable (Foldable)
import Data.Semigroup ((<>))
import Data.Vector.Generic (Vector)

import qualified Data.Foldable as Foldable
import qualified Data.Vector.Generic as Vector



-- | A list-like data structure with fast prepend and cache-friendly fold.
data Prependotron v a = Prependotron
  { bufferSize :: {-# UNPACK #-} !Word
  , buffer     :: ![a]
  , chunkSize  :: {-# UNPACK #-} !Word
  , chunks     :: ![v a] }

instance (Show a, Vector v a) => Show (Prependotron v a) where
  show xs = "fromFoldable " <> show (toList xs)

instance (Eq a, Vector v a) => Eq (Prependotron v a) where
  xs == ys = toList xs == toList ys

instance (Ord a, Vector v a) => Ord (Prependotron v a) where
  xs `compare` ys = toList xs `compare` toList ys



-- | /O(1)/ Create an empty prependotron with a given chunk size.
empty :: Word -> Prependotron v a
empty chunkSize' = Prependotron
  { bufferSize = 0
  , buffer     = []
  , chunkSize  = chunkSize'
  , chunks     = [] }
{-# INLINABLE empty #-}

-- | /O(n)/ Create a prependotron with a given chunk size from a foldable.
fromFoldable :: (Foldable f, Vector v a) => Word -> f a -> Prependotron v a
fromFoldable chunkSize' = Foldable.foldl' (flip cons) (empty chunkSize')
{-# INLINABLE fromFoldable #-}
{-# SPECIALIZE fromFoldable :: Vector v a => Word -> [a] -> Prependotron v a #-}



-- | /O(1)/ Prepend an element to the prependotron.
cons :: Vector v a => a -> Prependotron v a -> Prependotron v a
cons x xs
  | bufferSize xs + 1 == chunkSize xs =
      let newChunk = Vector.fromListN (w2i (chunkSize xs))
                                      (x : buffer xs) in
      let newXs = xs { bufferSize = 0
                     , buffer     = []
                     , chunks     = newChunk : chunks xs } in
      newChunk `par` newXs
  | otherwise =
      xs { bufferSize = bufferSize xs + 1
         , buffer     = x : buffer xs }
{-# INLINABLE cons #-}



-- | /O(n)/ Fold a prependotron using a function.
foldl' :: Vector v b => (a -> b -> a) -> a -> Prependotron v b -> a
foldl' f z xs =
  let foldedBuffer = Foldable.foldl' f z (buffer xs) in
  let foldedChunks = Foldable.foldl' (Vector.foldl' f) foldedBuffer (chunks xs) in
  foldedChunks
{-# INLINABLE foldl' #-}



w2i :: Word -> Int
w2i = fromIntegral
{-# INLINABLE w2i #-}

toList :: Vector v a => Prependotron v a -> [a]
toList = foldl' (flip (:)) []
{-# INLINABLE toList #-}
