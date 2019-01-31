module Stepik.Unit5.Monads.List where


lst = [(x,y) | x <- [1,2,3], y <- [1,2], x /= y]


lst' = do
  x <- [1,2,3]
  y <- [1,2]
  True <- return (x /= y) -- patern matching, քանի որ fail _ = [] ցուցակի դեպքոմ, անհաջող ելքի դեպքւոմ ուղակի այդ ճյուղը չի շարունակում
  return (x,y)

lst'' =
    [1,2,3]         >>= (\x ->
    [1,2]           >>= (\y ->
    return (x /= y) >>= (\b ->
    case b of True -> return (x,y)
              _    -> fail "..." )))


lst''' = do
  x <- [1,2,3]
  y <- [1,2]
  if (x /= y) then "Z" else []
  return (x,y)
