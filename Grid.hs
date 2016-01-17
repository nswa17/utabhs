module Grid where

data Grid = Grid {teams::[Int], cp1::Int, cp2::Int} deriving (Show, Read)

--instance Show Grid where
--  show g = (concat $ intersperse "|" $ map show $ teams g) ++ ":" ++ (show $ cp g)
--  --show g = show . cp $ g

instance Eq Grid where
  g == h = (teams g)  == (teams h)

instance Ord Grid where
  g1 `compare` g2 = (negate $ cp1 g1, negate $ cp2 g1) `compare` (negate $ cp1 g2, negate $ cp2 g2)

type Grids = [Grid]
