
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Build

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Build where

import Control.Monad.Except
import Control.Monad.Identity

import qualified    Data.ByteString as BS
import              Data.List       (intercalate)

import              Language.Qux.Annotated.Parser       hiding (parse)
import qualified    Language.Qux.Annotated.Parser       as P
import              Language.Qux.Annotated.Simplify
import              Language.Qux.Annotated.Syntax
import              Language.Qux.Annotated.TypeChecker
import qualified    Language.Qux.Llvm.Compiler          as C

import LLVM.General
import LLVM.General.Context

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error


data Options = Options {
    optCompile      :: Bool,
    optDestination  :: FilePath,
    optFormat       :: Format,
    optTypeCheck    :: Bool,
    argFilePaths    :: [FilePath]
    }

defaultOptions :: Options
defaultOptions = Options {
    optCompile      = False,
    optDestination  = "." ++ [pathSeparator],
    optFormat       = Bitcode,
    optTypeCheck    = False,
    argFilePaths    = []
    }

data Format = Assembly | Bitcode
    deriving Eq

instance Show Format where
    show Assembly   = "assembly"
    show Bitcode    = "bitcode"


handle :: Options -> IO ()
handle options = do
    let filePaths = argFilePaths options
    fileContents <- mapM readFile filePaths

    ethr <- catchIOError
        (runExceptT $ zipWithM parse filePaths fileContents >>= mapM_ (build options))
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

parse :: FilePath -> String -> ExceptT String IO (Program SourcePos)
parse filePath contents = mapExceptT (return . runIdentity) (withExcept show (P.parse program filePath contents))

build :: Options -> Program SourcePos -> ExceptT String IO ()
build options program = do
    when (optTypeCheck options) $ typeCheck program
    when (optCompile options)   $ compile options program

typeCheck :: Program SourcePos -> ExceptT String IO ()
typeCheck program = when (not $ null errors) $ throwError (intercalate "\n\n" $ map show errors)
    where
        errors = check program

compile :: Options -> Program SourcePos -> ExceptT String IO ()
compile options program
    | optFormat options == Assembly = liftIO $ do
        assembly <- withContext $ \context ->
            runExceptT (withModuleFromAST context mod moduleLLVMAssembly) >>= either fail return

        createDirectoryIfMissing True basePath
        writeFile (addExtension (basePath ++ baseName) "ll") assembly
    | optFormat options == Bitcode  = liftIO $ do
        bitcode <- withContext $ \context ->
            runExceptT (withModuleFromAST context mod moduleBitcode) >>= either fail return

        createDirectoryIfMissing True basePath
        BS.writeFile (addExtension (basePath ++ baseName) "bc") bitcode
    | otherwise                     = error $ "format not implemented `" ++ show (optFormat options) ++ "'"
    where
        module_     = let (Program _ module_ _) = program in map sId module_
        mod         = C.compile $ sProgram program
        basePath    = intercalate [pathSeparator] ([optDestination options] ++ init module_) ++ [pathSeparator]
        baseName    = last module_

