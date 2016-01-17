module Misc where
  
random :: Int -> Int -> Int
random i j = (i^3 + j^2 - i * j + j `div` 5) `mod` 12
