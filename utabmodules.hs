module Main where
import Data.List
import Control.Concurrent
import Control.Monad

main = do
  a <- getLine
  --b <- getLine
  let
    matchup1 = getMatchupDemo $ read a
  mapM_ print matchup1
  print $ gridAdoptness matchup1
  --mapM_ print $ getMatchupDemoBp $ read b
  putStrLn ""
  let
    matchup2 = getMatchupDemo2 $ read a
  mapM_ print matchup2
  print $ gridAdoptness matchup2
  putStrLn ""
  let
    matchup3 = getMatchupDemo3 $ read a
  mapM_ print matchup3
  print $ gridAdoptness matchup2
  putStrLn ""

waiting :: Int -> IO ()
waiting n = do
  putStrLn $ "waiting" ++ (replicate n '.')
  threadDelay 1000000
  waiting (n+1)

getMatchup :: (Grids -> Grids) -> MVar Bool -> MVar Grids -> Grids -> IO ()
getMatchup chooseAlg ref matchup grids = do
  putMVar matchup $! chooseAlg grids
  putStrLn "Done"
  putMVar ref True

threadGetMatchups grids = do
  alg1finished <- newMVar False
  matchup1 <- newMVar []
  alg2finished <- newMVar False
  matchup2 <- newMVar []
  alg3finished <- newMVar False
  matchup3 <- newMVar []

  w <- forkIO $ waiting 0
  u <- forkIO $ getMatchup chooseGridsBest alg1finished matchup1 grids
  v <- forkIO $ getMatchup chooseGridsNoWorse alg2finished matchup2 grids
  x <- forkIO $ getMatchup chooseGridsNoWorst alg3finished matchup3 grids

  {--
  testF matchup1

  where
    testF :: MVar Grids -> IO ()
    testF mvGrids = do
      threadDelay 1000000
      grids <- takeMVar mvGrids
      mapM_ print grids
      putStrLn "called"
      testF mvGrids
  --}
  
  go alg1finished matchup1
  go alg2finished matchup2 
  go alg3finished matchup3 
  killThread w

  where
    go :: MVar Bool -> MVar Grids -> IO ()
    go algfinished mvMatchup = do
      tf <- takeMVar algfinished
      matchup <- takeMVar mvMatchup
      case tf of
        True -> do
          putStrLn ""
          mapM_ print matchup
        False -> go algfinished mvMatchup

db = let gs = createGrids 300 in threadGetMatchups gs

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
chooseGridsNoWorse :: Grids -> Grids
chooseGridsNoWorse [] = []
chooseGridsNoWorse all@(g:gs) =
  let
    bestGrid = bestRelated all g
  in
    bestGrid:(chooseGridsNoWorse $ restGrids all bestGrid)

-- choose best in grids which have worst grid in all
chooseGridsNoWorst :: Grids -> Grids
chooseGridsNoWorst [] = []
chooseGridsNoWorst grids =
  let
    worstGrid = minimum grids
    bestGrid = bestRelated grids worstGrid
  in
    bestGrid:(chooseGridsNoWorst $ restGrids grids bestGrid)

getMatchupDemo :: Int -> Grids
getMatchupDemo = chooseGridsBest . createGrids

getMatchupDemo2 :: Int -> Grids
getMatchupDemo2 = chooseGridsNoWorse . createGrids

getMatchupDemo3 :: Int -> Grids
getMatchupDemo3 = chooseGridsNoWorst . createGrids

getMatchupDemoBp :: Int -> Grids
getMatchupDemoBp = chooseGridsBest . createGridsBp
