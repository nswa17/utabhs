module Main where
import Data.List

main = do
  a <- getLine
  --b <- getLine
  let
    matchup1 = getMatchupDemo $ read a
  mapM_ print matchup1
  putStrLn ""
  print $ gridAdoptness matchup1
  --mapM_ print $ getMatchupDemoBp $ read b
  putStrLn ""
  let
    matchup2 = getMatchupDemo2 $ read a
  mapM_ print matchup2
  putStrLn ""
  print $ gridAdoptness matchup2
  putStrLn ""

data Grid = Grid {teams::[Int], cp1::Int, cp2::Int} deriving (Show, Read)

--instance Show Grid where
--  show g = (concat $ intersperse "|" $ map show $ teams g) ++ ":" ++ (show $ cp g)
--  --show g = show . cp $ g

instance Eq Grid where
  g == h = (teams g)  == (teams h)

instance Ord Grid where
  g1 `compare` g2 = (negate $ cp1 g1, negate $ cp2 g1) `compare` (negate $ cp1 g2, negate $ cp2 g2)

related g h = (length $ nub $ (teams g) ++ (teams h)) /= (length (nub $ teams g) + length (nub $ teams h))

type Grids = [Grid]

random i j = (i^3 + j^2 - i * j + j `div` 5) `mod` 12

gridAdoptness :: Grids -> (Int, Int)
gridAdoptness grids = ((sum $ map cp1 grids) `div` (length grids), (sum $ map cp2 grids) `div` (length grids))

createGrids :: Int -> Grids
--createGrids n = (\i j -> Grid [i, j] $ random i j) <$> [1..n] <*> [1..n]
createGrids n = [Grid [i, j] (random i j) (random j i) | i <- [1..n], j <- [1..n], i > j]
createGridsBp n = [Grid [i, j, k, l] (random i j) (random j i) | i <- [1..n], j <- [1..n], k <- [1..n], l <- [1..n], i > j, j > k, k > l]

restGrids :: Grids -> Grid -> Grids
restGrids [] _ = []
restGrids (g:gs) grid
  | related g grid = rest
  | otherwise = g:rest
  where rest = restGrids gs grid

chooseGrids :: Grids -> Grids
chooseGrids [] = []
chooseGrids (g:gs) = g:(chooseGrids $ restGrids gs g)

sortByNothing :: Grids -> Grids
sortByNothing = id

-- choose grids by given order
chooseGridsNothing :: Grids -> Grids
chooseGridsNothing = chooseGrids . sortByNothing

-- choose grids best by best
chooseGridsBest :: Grids -> Grids
chooseGridsBest = chooseGrids . sort

bestRelated grids grid = head $ sort $ relatedGrids grids grid

relatedGrids [] _ = []
relatedGrids (g:gs) grid
  | related g grid = g:rest
  | otherwise = rest
  where rest = relatedGrids gs grid

-- choose grids which are not extremely bad
chooseGridsNoWorst :: Grids -> Grids
chooseGridsNoWorst [] = []
chooseGridsNoWorst all@(g:gs) = let bestGrid = bestRelated all g in bestGrid:(chooseGridsNoWorst $ restGrids all bestGrid)

getMatchup :: (Grids -> Grids) -> Grids -> Grids
getMatchup chooseAlg grids = chooseAlg grids

getMatchupDemo :: Int -> Grids
getMatchupDemo = getMatchup chooseGridsBest . createGrids

getMatchupDemo2 :: Int -> Grids
getMatchupDemo2 = getMatchup chooseGridsNoWorst . createGrids 

getMatchupDemoBp :: Int -> Grids
getMatchupDemoBp = getMatchup chooseGridsBest . createGridsBp