module BottomUpMergeSort where

import Control.Monad.Writer
import Data.Foldable (foldlM)

type Less a = a -> a -> Bool

data Sortable a = Sortable { _less :: Less a
                           , _size :: Int
                           , _segments :: [[a]] }

instance Show a =>  Show (Sortable a) where
    show (Sortable _ s segs) = "Sortable { _size = " ++ show s
                               ++ ", _segments = " ++ show segs

type ComputationLog a = Writer [String] a

merge :: Less a -> [a] -> [a] -> [a]
merge less = mrg
    where
      mrg [] ys = ys
      mrg xs [] = xs
      mrg (x:xs) (y:ys) = if less x y
                          then x : mrg xs (y:ys)
                          else y : mrg (x:xs) ys

new :: Less a -> ComputationLog (Sortable a)
new less = return $ Sortable { _less = less
                             , _size = 0
                             , _segments = [] }

add :: (Show a) => a -> Sortable a -> ComputationLog (Sortable a)
add x (Sortable l size segs) = do
  let res = Sortable l (size + 1) (addSeg [x] segs size)
  tell $ ["Adding " ++ show x]
  tell $ [show res]
  return res
    where
      addSeg seg segs' size' =
          if size' `mod` 2 == 0
          then seg : segs'
          else addSeg (merge l seg (head segs')) (tail segs') (size' `div` 2)

sort :: Sortable a -> [a]
sort (Sortable l _ segs) = mergeAll [] segs
    where
      mergeAll xs [] = xs
      mergeAll xs (seg:segs') = mergeAll (merge l xs seg) segs'

fromList :: (Ord a, Show a)
         => [a] -> ComputationLog (Sortable a)
fromList xs = (new (<)) >>= \initial -> foldlM (flip add) initial xs

runFromList :: (Ord a, Show a)
            => [a] -> IO ()
runFromList xs = do
  let (sortable, compLog) = runWriter $ fromList xs
  putStrLn "Sortable:"
  print sortable
  putStrLn "Log:"
  forM_ compLog $ \l -> do
    putStrLn l
