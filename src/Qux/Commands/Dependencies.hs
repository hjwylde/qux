
{-|
Module      : Qux.Commands.Dependencies

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Dependencies where

import Control.Monad.Except

import Data.List.Extra (nubOrd, sort)

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import qualified Qux.Commands.Build as Build

import System.Exit
import System.IO
import System.IO.Error


data Options = Options {
    argFilePaths :: [FilePath]
    }
    deriving (Eq, Show)


handle :: Options -> IO ()
handle options = do
    let filePaths = argFilePaths options
    fileContents <- mapM readFile filePaths

    ethr <- catchIOError
        (runExceptT $ zipWithM Build.parse filePaths fileContents >>= dependencies options)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

dependencies :: Options -> [Program SourcePos] -> ExceptT String IO ()
dependencies _ programs = liftIO $ mapM_ putStrLn imports
    where
        imports = nubOrd $ sort [simp (qualify id) |
            (Program _ _ decls) <- programs,
            (ImportDecl _ id) <- decls
            ]

