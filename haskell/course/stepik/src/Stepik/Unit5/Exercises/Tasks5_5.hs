{-# LANGUAGE OverloadedStrings #-}
module Stepik.Unit5.Exercises.Tasks5_5 where

import Control.Monad
import Data.List        (isInfixOf)
import System.Directory
-------------------------------------------------------------------------------
-- Exercise 5.5.2

-- |
-- В этом задании ваша программа должна попросить пользователя ввести любую строку,
-- а затем удалить все файлы в текущей директории, в именах которых содержится эта строка,
-- выдавая при этом соответствующие сообщения.

main = do
  putStr "Substring: "
  subStr <- getLine
  removeFilesBySubstring "/dirrest" subStr

removeFilesBySubstring :: FilePath -> String -> IO ()
removeFilesBySubstring path subStr = do
--  getDirectoryContents path >>= mapM_ removeFile . filterM (return $ isInfixOf subStr)
  withCurrentDirectory path $ do
                       mapM_ (\f -> removeFile f >> putStrLn ("Removing file: " ++ f)) . filter (isInfixOf subStr) =<< getDirectoryContents path
