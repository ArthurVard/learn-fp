module RWH.Ch18.CountEntries (listDirectory, countEntriesTrad) where

import Control.Monad    (forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  ((</>))


listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest


-- |
data Expr a = Number     Int
            | Boolean    Bool
            | Increment (Expr Int)
            | Not       (Expr Bool)


type Ide = String
data Raw = INT Int | ADD Raw Raw
         | VAR Ide | LAM Ide Raw | APP Raw Raw
           deriving (Show)

newtype Exp t = E ([Ide] -> Raw)

make :: Exp t -> [Ide] -> Raw
make (E a) ns = a ns

int :: Int -> Exp Int
int i = E (\ns -> INT i)

add :: Exp Int -> Exp Int -> Exp Int
add a b = E (\ns -> ADD (make a ns) (make b ns))

lam :: (Exp a -> Exp b) -> Exp (a -> b)
lam f = E (\(n:ns) -> LAM n (make (f (E (\z -> VAR n))) ns))

app :: Exp (a -> b) -> Exp a -> Exp b
app a b = E (\ns -> APP (make a ns) (make b ns))

instance Show (Exp t) where
    showsPrec i (E a) = showsPrec i r
        where
          r = a [c : i | i <- ("" : map show [1..]), c <- ['a'..'z']]


-- | the problem
-- ex1 = app (int 1) (lam (\x -> x))

ex2 :: (Integer -> Integer) -> Integer
ex2 = \f -> 2 + (f 1)
