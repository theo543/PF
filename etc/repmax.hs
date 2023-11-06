import Debug.Trace

repMax :: [Int] -> Int -> (Int, [Int])
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (l : ls) rep = (m', rep : ls')
  where (m, ls') = repMax ls rep
        m' = max m l

doRepMax :: [Int] -> [Int]
doRepMax xs = xs'
  where (largest, xs') = repMax xs $ trace ("Got: " ++ show largest) largest
