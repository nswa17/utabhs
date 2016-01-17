module ArrangeGrid where
import Grid
import Data.List
import qualified Misc

related :: Grid -> Grid -> Bool
related g h = (length $ nub $ (teams g) ++ (teams h)) /= (length (nub $ teams g) + length (nub $ teams h))

gridAdoptness :: Grids -> (Int, Int)
gridAdoptness grids = ((sum $ map cp1 grids) `div` (length grids), (sum $ map cp2 grids) `div` (length grids))

createGrids :: Int -> Grids
--createGrids n = (\i j -> Grid [i, j] $ random i j) <$> [1..n] <*> [1..n]
createGrids n = [Grid [i, j] (Misc.random i j) (Misc.random j i) | i <- [1..n], j <- [1..n], i > j]

createGridsBp :: Int -> Grids
createGridsBp n = [Grid [i, j, k, l] (Misc.random i j) (Misc.random j i) | i <- [1..n], j <- [1..n], k <- [1..n], l <- [1..n], i > j, j > k, k > l]
