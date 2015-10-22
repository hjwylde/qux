
{-|
Module      : Qux.Test.Steps

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Test.Steps (
    clean, build,

    actualOutputFilePath, expectedOutputFilePath,
) where

import Control.Monad.Except
import Control.Monad.Extra

import Pipes

import Qux.Commands.Build as Build
import Qux.Worker

import System.Directory
import System.Exit
import System.FilePath
import System.Process


clean :: FilePath -> IO ()
clean dir = do
    whenM (doesDirectoryExist binDir)       $ removeDirectoryRecursive binDir
    whenM (doesDirectoryExist distDir)      $ removeDirectoryRecursive distDir
    whenM (doesFileExist outputFilePath)    $ removeFile outputFilePath
    where
        outputFilePath  = actualOutputFilePath dir
        binDir          = dir </> "bin"
        distDir         = dir </> "dist"

build :: FilePath -> IO ()
build dir = void $ runExceptT (runEffect $ for worker appendOutputFile)
    where
        worker                  = prepare dir >> compile dir >> link dir >> run dir
        appendOutputFile str    = liftIO $ appendFile (actualOutputFilePath dir) (str ++ "\n")

prepare :: FilePath -> WorkerT IO ()
prepare dir = liftIO $ writeFile (actualOutputFilePath dir) ""

compile :: FilePath -> WorkerT IO ()
compile dir = do
    liftIO $ createDirectoryIfMissing True binDir

    findFilesByExtension [".qux"] libDir    >>= \files -> compileQux files []
    findFilesByExtension [".c"] libDir      >>= compileC
    findFilesByExtension [".qux"] srcDir    >>= \files -> compileQux files [libDir]
    findFilesByExtension [".c"] srcDir      >>= compileC
    where
        compileQux filePaths libdirs = do
            Build.handle defaultOptions {
                optCompile      = True,
                optDestination  = binDir,
                optLibdirs      = libdirs,
                optTypeCheck    = True,
                argFilePaths    = filePaths
                }
            mapM_ (\filePath ->
                runProcessForWorker "llc" ["-filetype", "obj", "-o", replaceDirectory filePath binDir -<.> "o", filePath] "")
                =<< findFilesByExtension [".bc"] binDir

        compileC filePaths = mapM_
            (\filePath ->
                runProcessForWorker "gcc" ["-c", "-o", replaceDirectory filePath binDir -<.> "o", filePath] "")
            filePaths

        binDir          = dir </> "bin"
        libDir          = dir </> "lib"
        srcDir          = dir </> "src"

link :: FilePath -> WorkerT IO ()
link dir = do
    filePaths <- findFilesByExtension [".o"] binDir

    liftIO $ createDirectoryIfMissing True distDir

    runProcessForWorker "gcc" (["-o", distDir </> "main"] ++ filePaths) ""
    where
        binDir  = dir </> "bin"
        distDir = dir </> "dist"

run :: FilePath -> WorkerT IO ()
run dir = runProcessForWorker (dir </> "dist" </> "main") [] ""


-- Helper methods

runProcessForWorker :: FilePath -> [String] -> String -> WorkerT IO ()
runProcessForWorker cmd args input = do
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args input

    unless (null $ stdout ++ stderr)    $ yield (stdout ++ stderr)
    when (exitCode /= ExitSuccess)      $ throwError exitCode


actualOutputFilePath :: FilePath -> FilePath
actualOutputFilePath dir = dir </> "output" <.> "txt"

expectedOutputFilePath :: FilePath -> FilePath
expectedOutputFilePath dir = dir </> "expected-output" <.> "txt"

findFilesByExtension :: [String] -> FilePath -> WorkerT IO [FilePath]
findFilesByExtension exts dir = liftIO $ do
    filePaths <- ifM (doesDirectoryExist dir)
        (filter ((`elem` exts) . takeExtension) <$> getDirectoryContents dir)
        (return [])

    return $ map (combine dir) filePaths

