{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com
-}

module Main (
    main
) where

import Control.Monad

import Qux.Test.Integration
import Qux.Test.Steps

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
    testsDir    <- getCurrentDirectory >>= \dir -> return $ dir </> "test" </> "build" </> "tests"
    testDirs    <- filter ((/= '.') . head) <$> getDirectoryContents testsDir
    testTrees   <- mapM test =<< filterM
        (\testDir -> not <$> doesFileExist (testDir </> "pending"))
        (map (combine testsDir) testDirs)

    return $ testGroup "Tests" testTrees

test :: String -> IO TestTree
test dir = do
    let name = takeFileName dir

    clean dir

    return $ goldenVsFile name
        (expectedOutputFilePath dir)
        (actualOutputFilePath dir)
        (withCurrentDirectory dir $ build ".")
