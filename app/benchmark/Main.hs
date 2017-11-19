{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import Data.Prependotron

import Control.Monad (join)
import Criterion.Main (bench, defaultMain, whnf)
import Data.Semigroup ((<>))
import Data.Vector.Unboxed (Vector)

import qualified Data.Foldable as Foldable

main :: IO ()
main = defaultMain . join $
  [ [ bench ("buildFoldList $ " <> show testSize) $
        whnf buildFoldList testSize ]
  , join [ [ bench ("buildFoldPrependotronPar $ " <> show chunkSize) $
               whnf (buildFoldPrependotronPar) chunkSize
           , bench ("buildFoldPrependotronSeq $ " <> show chunkSize) $
               whnf (buildFoldPrependotronSeq) chunkSize
           , bench ("buildFoldPrependotronLazy $ " <> show chunkSize) $
               whnf (buildFoldPrependotronLazy) chunkSize ]
         | chunkSize <- [512, 1024, 2048, 4096] ] ]

buildFoldList :: Word -> Word
buildFoldList n = go n []
  where
    go :: Word -> [Word] -> Word
    go i xs | i <= 0    = Foldable.foldl' function 1 xs
            | otherwise = go (i - 1) (i : xs)

#define BUILD_FOLD_PREPENDOTRON(name, conslike)                              \
  name :: Word -> Word                                                      ;\
  name chunkSize =                                                           \
    go testSize (empty chunkSize)                                            \
    where                                                                   {\
      go :: Word -> Prependotron Vector Word -> Word                        ;\
      go i xs | i <= 0    = foldl' function 1 xs                             \
              | otherwise = go (i - 1) (conslike i xs)                      }
BUILD_FOLD_PREPENDOTRON(buildFoldPrependotronPar , consPar )
BUILD_FOLD_PREPENDOTRON(buildFoldPrependotronSeq , consSeq )
BUILD_FOLD_PREPENDOTRON(buildFoldPrependotronLazy, consLazy)
#undef BUILD_FOLD_PREPENDOTRON

function :: Word -> Word -> Word
function x y = x + y + x - y + x * y

testSize :: Word
testSize = 5000000
