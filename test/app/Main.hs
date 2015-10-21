
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main
) where

import Qux.Test.Steps

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.Golden


main :: IO ()
main = defaultMain =<< tests


tests :: IO TestTree
tests = do
    testsDir    <- getCurrentDirectory >>= \dir -> return $ dir </> "test" </> "tests"
    testDirs    <- getDirectoryContents testsDir
    testTrees   <- mapM (test . combine testsDir) (filter ((/= '.') . head) testDirs)

    return $ testGroup "Tests" testTrees

test :: String -> IO TestTree
test dir = do
    let name = takeDirectory dir

    clean dir

    return $ goldenVsFile name
        (expectedOutputFilePath dir)
        (actualOutputFilePath dir)
        (compile dir >> link dir >> run dir)

