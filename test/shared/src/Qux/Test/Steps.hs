{-|
Module      : Qux.Test.Steps

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Test.Steps (
    clean, build,
) where

import Control.Monad.Except
import Control.Monad.Extra

import           Pipes
import qualified Pipes.Prelude as Pipes
import           Prelude       hiding (log)

import Qux.Commands.Build   as Build
import Qux.Test.Integration
import Qux.Worker

import System.Directory
import System.FilePath
import System.IO

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
build dir = withBinaryFile (actualOutputFilePath dir) WriteMode $ \handle ->
    void $ runExceptT (runEffect $ worker >-> requirePriority Info >-> extractMessage >-> Pipes.toHandle handle)
    where
        worker          = compile dir >> link dir >> run dir
        extractMessage  = Pipes.map snd

compile :: FilePath -> WorkerT IO ()
compile dir = do
    liftIO $ createDirectoryIfMissing True binDir

    findFilesByExtension [".qux"] libDir    >>= compileQux []
    findFilesByExtension [".c"] libDir      >>= compileC
    findFilesByExtension [".qux"] srcDir    >>= compileQux [libDir]
    findFilesByExtension [".c"] srcDir      >>= compileC
    where
        compileQux libdirs filePaths = do
            Build.handle defaultOptions {
                optCompile      = True,
                optDestination  = binDir,
                optLibdirs      = libdirs,
                optTypeCheck    = True,
                argFilePaths    = filePaths
                }

            mapM_ (\filePath ->
                runProcess "llc" ["-filetype", "obj", "-o", replaceDirectory filePath binDir -<.> "o", filePath] "")
                =<< findFilesByExtension [".bc"] binDir

        compileC = mapM_ $ \filePath -> runProcess "gcc" ["-c", "-o", replaceDirectory filePath binDir -<.> "o", filePath] ""

        binDir = dir </> "bin"
        libDir = dir </> "lib"
        srcDir = dir </> "src"

link :: FilePath -> WorkerT IO ()
link dir = do
    filePaths <- findFilesByExtension [".o"] binDir

    liftIO $ createDirectoryIfMissing True distDir

    runProcess "gcc" (["-o", distDir </> "main"] ++ filePaths) ""
    where
        binDir  = dir </> "bin"
        distDir = dir </> "dist"

run :: FilePath -> WorkerT IO ()
run dir = runProcess (dir </> "dist" </> "main") [] ""
