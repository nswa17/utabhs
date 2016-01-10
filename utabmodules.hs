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

data Grid = Grid {teams::[Int], cp::Int}

instance Show Grid where
  show g = (concat $ intersperse "|" $ map show $ teams g) ++ ":" ++ (show $ cp g)
  --show g = show . cp $ g

instance Eq Grid where
  g == h = (teams g)  == (teams h)

related g h = (length $ nub $ (teams g) ++ (teams h)) /= (length (nub $ teams g) + length (nub $ teams h))

type Grids = [Grid]

random i j = (i^3 + j^2 - i * j + j `div` 2) `mod` 12

gridAdoptness :: Grids -> Int
gridAdoptness grids = (sum $ map cp grids) `div` (length grids)

createGrids :: Int -> Grids
--createGrids n = (\i j -> Grid [i, j] $ random i j) <$> [1..n] <*> [1..n]
createGrids n = [Grid [i, j] $ random i j | i <- [1..n], j <- [1..n], i > j]
createGridsBp n = [Grid [i, j, k, l] $ random i j | i <- [1..n], j <- [1..n], k <- [1..n], l <- [1..n], i > j, j > k, k > l]

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

sortByCp :: Grids -> Grids
sortByCp grids = sortOn (negate . cp) grids
sortByCpRev grids = sortOn cp grids

-- choose grids by given order
chooseGridsNothing :: Grids -> Grids
chooseGridsNothing = chooseGrids . sortByNothing

-- choose grids best by best
chooseGridsBest :: Grids -> Grids
chooseGridsBest = chooseGrids . sortByCp

bestRelated grids grid = head $ sortByCp $ relatedGrids grids grid

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