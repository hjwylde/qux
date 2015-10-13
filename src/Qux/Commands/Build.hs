
{-|
Module      : Qux.Commands.Build

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Build where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

import qualified    Data.ByteString as BS
import              Data.List       (intercalate)

import qualified    Language.Qux.Annotated.NameResolver     as NameResolver
import              Language.Qux.Annotated.Parser           hiding (parse)
import qualified    Language.Qux.Annotated.Parser           as P
import              Language.Qux.Annotated.Syntax
import              Language.Qux.Annotated.TypeChecker
import qualified    Language.Qux.Annotated.TypeResolver     as TypeResolver
import qualified    Language.Qux.Llvm.Compiler              as C

import LLVM.General
import LLVM.General.Context hiding (Context)

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
    deriving (Eq, Show)

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
        (runExceptT $ zipWithM parse filePaths fileContents >>= build options)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

parse :: FilePath -> String -> ExceptT String IO (Program SourcePos)
parse filePath contents = mapExceptT (return . runIdentity) (withExcept show (P.parse program filePath contents))

build :: Options -> [Program SourcePos] -> ExceptT String IO ()
build options unresolvedPrograms = do
    let context = baseContext $ map simp unresolvedPrograms

    programs <- mapM (resolve context) unresolvedPrograms

    when (optTypeCheck options) $ mapM_ (\program -> typeCheck (narrowContext context (simp program)) program) programs
    when (optCompile options)   $ mapM_ (\program -> compile options (narrowContext context (simp program)) program) programs

resolve :: Context -> Program SourcePos -> ExceptT String IO (Program SourcePos)
resolve baseContext program = do
    let (program', errors') = NameResolver.runResolve (NameResolver.resolveProgram program) context
    when (not $ null errors') $ throwError (intercalate "\n\n" $ map show errors')

    let (program'', errors'') = TypeResolver.runResolve (TypeResolver.resolveProgram program') context
    when (not $ null errors'') $ throwError (intercalate "\n\n" $ map show errors'')

    return program''
    where
        context = narrowContext baseContext (simp program)

typeCheck :: Context -> Program SourcePos -> ExceptT String IO ()
typeCheck context program = when (not $ null errors) $ throwError (intercalate "\n\n" $ map show errors)
    where
        errors = execCheck (checkProgram program) context

compile :: Options -> Context -> Program SourcePos -> ExceptT String IO ()
compile options context program
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
    | otherwise                     = error $ "internal error: format not implemented `" ++ show (optFormat options) ++ "'"
    where
        module_     = let (Program _ module_ _) = program in map simp module_
        mod         = runReader (C.compileProgram $ simp program) context
        basePath    = intercalate [pathSeparator] ([optDestination options] ++ init module_) ++ [pathSeparator]
        baseName    = last module_

