
{-|
Module      : Qux.Test.Steps

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Test.Steps (
    clean, compile, link, run,

    actualOutputFilePath, expectedOutputFilePath,
) where

import Control.Monad.Extra

import Qux.Commands.Build as Build

import System.Directory
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

compile :: FilePath -> IO ()
compile dir = do
    createDirectoryIfMissing True binDir

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
            mapM_ (\filePath -> (do
                callCommand $ unwords ["llc -filetype obj", "-o", quote $ replaceDirectory filePath binDir -<.> "o", quote filePath]))
                =<< findFilesByExtension [".bc"] binDir

        compileC filePaths = mapM_
            (\filePath ->
                callCommand $ unwords ["gcc -c", "-o", quote $ replaceDirectory filePath binDir -<.> "o", quote filePath])
            filePaths

        binDir = dir </> "bin"
        libDir = dir </> "lib"
        srcDir = dir </> "src"

link :: FilePath -> IO ()
link dir = do
    filePaths <- findFilesByExtension [".o"] binDir

    createDirectoryIfMissing True distDir

    callCommand $ unwords (["gcc", "-o", quote $ distDir </> "main"] ++ map quote filePaths)
    where
        binDir  = dir </> "bin"
        distDir = dir </> "dist"

run :: FilePath -> IO ()
run dir = callCommand $ unwords [dir </> "dist" </> "main", "&>", actualOutputFilePath dir]


actualOutputFilePath :: FilePath -> FilePath
actualOutputFilePath dir = dir </> "output" <.> "txt"

expectedOutputFilePath :: FilePath -> FilePath
expectedOutputFilePath dir = dir </> "expected-output" <.> "txt"

findFilesByExtension :: [String] -> FilePath -> IO [FilePath]
findFilesByExtension exts dir = do
    filePaths <- ifM (doesDirectoryExist dir)
        (filter ((`elem` exts) . takeExtension) <$> getDirectoryContents dir)
        (return [])

    return $ map (combine dir) filePaths

-- TODO (hjw): replace any quote marks in str
quote :: String -> String
quote str = "\"" ++ str ++ "\""

