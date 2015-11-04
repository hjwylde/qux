
{-|
Module      : Qux.Test.Integration

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Test.Integration (
    runProcess,

    withCurrentDirectory, findFilesByExtension,

    actualOutputFilePath, expectedOutputFilePath,
) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Extra

import Prelude hiding (log)

import Qux.Worker

import System.Directory
import System.Exit
import System.FilePath
import System.Process   hiding (runProcess)


runProcess :: FilePath -> [String] -> String -> WorkerT IO ()
runProcess cmd args input = do
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args input

    unless (null stdout)            $ log Info stdout
    unless (null stderr)            $ log Error stderr
    when (exitCode /= ExitSuccess)  $ throwError exitCode


withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = bracket getCurrentDirectory setCurrentDirectory $ \_ -> setCurrentDirectory dir >> action

findFilesByExtension :: [String] -> FilePath -> WorkerT IO [FilePath]
findFilesByExtension exts dir = liftIO $ do
    filePaths <- ifM (doesDirectoryExist dir)
        (filter ((`elem` exts) . takeExtension) <$> getDirectoryContents dir)
        (return [])

    return $ map (combine dir) filePaths


actualOutputFilePath :: FilePath -> FilePath
actualOutputFilePath dir = dir </> "output" <.> "txt"

expectedOutputFilePath :: FilePath -> FilePath
expectedOutputFilePath dir = dir </> "expected-output" <.> "txt"

