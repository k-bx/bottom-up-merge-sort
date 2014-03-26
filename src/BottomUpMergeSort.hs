module BottomUpMergeSort where

type Less a = a -> a -> Bool

data Sortable a = Sortable { _less :: Less a
                           , _size :: Int
                           , _segments :: [[a]] }

instance Show a =>  Show (Sortable a) where
    show (Sortable _ s segs) = "Sortable { _size = " ++ show s
                               ++ ", _segments = " ++ show segs

merge :: Less a -> [a] -> [a] -> [a]
merge less = mrg
    where
      mrg [] ys = ys
      mrg xs [] = xs
      mrg (x:xs) (y:ys) = if less x y
                          then x : mrg xs (y:ys)
                          else y : mrg (x:xs) ys

new :: Less a -> Sortable a
new less = Sortable { _less = less
                    , _size = 0
                    , _segments = [] }

add :: a -> Sortable a -> Sortable a
add x (Sortable l size segs) = Sortable l (size + 1) (addSeg [x] segs size)
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

fromList :: (Ord a) => [a] -> Sortable a
fromList = foldr add (new (<))
