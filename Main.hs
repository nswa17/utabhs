module Main where
import Control.Concurrent
import Control.Monad
import qualified Socket
import qualified Select
import qualified ArrangeGrid
import Grid
{--

Main--Socket
  |
  |___Select
  |     |___ArrangeGrid
  |     |___Grid
  |
  |___ArrangeGrid
  |     |___Grid
  |     |___Misc
  |
  |___Grid





--}




{--
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
--}

main = do
  threadGetMatchups $ ArrangeGrid.createGrids 500

waiting :: Int -> IO ()
waiting n = do
  putStrLn $ "waiting" ++ (replicate n '.')
  threadDelay 1000000
  waiting (n+1)

getMatchup :: (Grids -> Grids) -> MVar Bool -> MVar Grids -> Grids -> IO ()
getMatchup chooseAlg ref matchup grids = do
  let
      chosenGrids = chooseAlg grids
  chosenGrids `seq` (putMVar matchup chosenGrids)
  putStrLn "Done"
  print $ length chosenGrids
  putMVar ref True

threadGetMatchups grids = do
  alg1finished <- newMVar False
  matchup1 <- newMVar []
  alg2finished <- newMVar False
  matchup2 <- newMVar []
  alg3finished <- newMVar False
  matchup3 <- newMVar []

  w <- forkIO $ waiting 0
  u <- forkIO $ getMatchup Select.chooseGridsBest alg1finished matchup1 grids
  v <- forkIO $ getMatchup Select.chooseGridsNoWorse alg2finished matchup2 grids
  x <- forkIO $ getMatchup Select.chooseGridsNoWorst alg3finished matchup3 grids

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
      print $ length matchup
      case tf of
        True -> do
          putStrLn ""
          --mapM_ print matchup
        False -> go algfinished mvMatchup

db = let gs = ArrangeGrid.createGrids 300 in threadGetMatchups gs
