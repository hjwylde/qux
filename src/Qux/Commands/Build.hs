
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

-- TODO (hjw): simplify all programs as early as possible
-- TODO (hjw): re-use contexts as much as possible

handle :: Options -> IO ()
handle options = do
    let filePaths = argFilePaths options
    fileContents <- mapM readFile filePaths

    ethr <- catchIOError
        (runExceptT $ zipWithM parse filePaths fileContents >>= resolveNames >>= resolveTypes >>= build options)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

parse :: FilePath -> String -> ExceptT String IO (Program SourcePos)
parse filePath contents = mapExceptT (return . runIdentity) (withExcept show (P.parse program filePath contents))

resolveNames :: [Program SourcePos] -> ExceptT String IO [Program SourcePos]
resolveNames programs = mapM resolve programs
    where
        resolve :: Program SourcePos -> ExceptT String IO (Program SourcePos)
        resolve program = do
            let context'            = NameResolver.context (simp program) (map simp programs)
            let (program', errors)  = NameResolver.runResolve (NameResolver.resolveProgram program) context'

            when (not $ null errors) $ throwError (intercalate "\n\n" $ map show errors)

            return program'

resolveTypes :: [Program SourcePos] -> ExceptT String IO [Program SourcePos]
resolveTypes programs = mapM resolve programs
    where
        baseContext' = baseContext (map simp programs)
        resolve :: Program SourcePos -> ExceptT String IO (Program SourcePos)
        resolve program@(Program _ m _) = return $ TypeResolver.runResolve (TypeResolver.resolveProgram program) (baseContext' { module_ = map simp m })

build :: Options -> [Program SourcePos] -> ExceptT String IO ()
build options programs = do
    let baseContext' = baseContext $ map simp programs

    when (optTypeCheck options) $ mapM_ (\p@(Program _ m _) -> typeCheck (baseContext' { module_ = map simp m }) p) programs
    when (optCompile options)   $ mapM_ (\p@(Program _ m _) -> compile options (baseContext' { module_ = map simp m }) p) programs

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

