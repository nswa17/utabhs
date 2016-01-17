module Select where
import Grid
import Data.List
import qualified ArrangeGrid

restGrids :: Grids -> Grid -> Grids
restGrids [] _ = []
restGrids (g:gs) grid
  | ArrangeGrid.related g grid = rest
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

bestRelated :: Grids -> Grid -> Grid
bestRelated grids grid = head $ sort $ relatedGrids grids grid

relatedGrids :: Grids -> Grid -> Grids
relatedGrids [] _ = []
relatedGrids (g:gs) grid
  | ArrangeGrid.related g grid = g:rest
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
getMatchupDemo = chooseGridsBest . ArrangeGrid.createGrids

getMatchupDemo2 :: Int -> Grids
getMatchupDemo2 = chooseGridsNoWorse . ArrangeGrid.createGrids

getMatchupDemo3 :: Int -> Grids
getMatchupDemo3 = chooseGridsNoWorst . ArrangeGrid.createGrids

getMatchupDemoBp :: Int -> Grids
getMatchupDemoBp = chooseGridsBest . ArrangeGrid.createGridsBp
