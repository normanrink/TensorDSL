
module Utility ( showUnquoted
               , skipPair
               , swapPair
               , splitList
               , insertIntoList
               , insertPairIntoList ) where


showUnquoted :: (Show a) => a -> String
showUnquoted s = tail . init $ show s

skip :: Int -> [a] -> [a]
skip i ls = take i ls ++ drop (i+1) ls
  
skipPair :: (Int, Int) -> [a] -> [a]
skipPair (i0, i1) ls | i0 == i1  = skip i0 ls 
                     | otherwise = let i0' = min i0 i1
                                       i1' = max i0 i1
                                   in skip (i1'-1) $ skip i0' ls

swapPair :: (Int, Int) -> [a] -> [a]
swapPair (i0, i1) ls | i0 == i1  = ls
                     | otherwise =
                         let i0'    = min i0 i1
                             i1'    = max i0 i1
                             l0     = ls !! i0'
                             l1     = ls !! i1'
                             head   = take i0' ls
                             tail   = drop (i0'+1) ls
                             middle = take (i1'-i0'-1) tail
                             end    = drop (i1'-i0') tail
                         in head ++ [l1] ++ middle ++ [l0] ++ end

splitList :: Int -> [a] -> ([a], [a])
splitList i ls = let head = take i ls
                     tail = drop i ls
                 in (head, tail)

insertIntoList :: Int -> a -> [a] -> [a]
insertIntoList i l ls = let (head, tail) = splitList i ls
                        in head ++ [l] ++ tail

insertPairIntoList :: (Int, Int) -> (a, a) -> [a] -> [a]
insertPairIntoList (i0, i1) (l0, l1) ls =
  let i0' = min i0 i1
      i1' = max i0 i1
  in insertIntoList i1' l1 $ insertIntoList i0' l0 ls
