{-|
Module      : Qux.BuildSteps
Description : Build step utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Build step utilities.
-}

module Qux.BuildSteps (
    -- * Parsing
    parse, parseAll,

    -- * Resolving
    resolve,

    -- * Type checking
    typeCheck,

    -- * Compiling
    compileToLlvmAssembly, compileToLlvmBitcode,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader

import qualified Data.ByteString as BS
import           Data.List.Extra

import qualified Language.Qux.Annotated.NameResolver as NameResolver
import           Language.Qux.Annotated.Parser       hiding (parse)
import qualified Language.Qux.Annotated.Parser       as Parser
import           Language.Qux.Annotated.Syntax
import           Language.Qux.Annotated.TypeChecker
import qualified Language.Qux.Annotated.TypeResolver as TypeResolver
import qualified Language.Qux.Llvm.Compiler          as Compiler

import LLVM.General
import LLVM.General.Context hiding (Context)

import Prelude hiding (log)

import Qux.Worker

import System.Directory.Extra
import System.Exit
import System.FilePath

-- | Parses the file.
--   Returns the program if successful or yields the error message(s).
parse :: FilePath -> WorkerT IO (Program SourcePos)
parse filePath = do
    unlessM (liftIO $ doesFileExist filePath) $ do
        log Error $ "Cannot find file " ++ filePath
        throwError $ ExitFailure 1

    contents <- liftIO $ readFile filePath

    case runExcept (Parser.parse program filePath contents) of
        Left error      -> log Error (show error) >> throwError (ExitFailure 1)
        Right program   -> return program

-- | Parses the files.
--   Returns the programs if successful or yields the error message(s).
parseAll :: [FilePath] -> WorkerT IO [Program SourcePos]
parseAll = mapM parse

-- | Resolves the program with the given base context.
--   Returns the program if successful or yields the error message(s).
resolve :: Context -> Program SourcePos -> WorkerT IO (Program SourcePos)
resolve baseContext program = do
    let (program', errors') = NameResolver.runResolve (NameResolver.resolveProgram program) context
    unless (null errors') $ do
        report Error $ intersperse "" (map show errors')
        throwError $ ExitFailure 1

    let (program'', errors'') = TypeResolver.runResolve (TypeResolver.resolveProgram program') context
    unless (null errors'') $ do
        report Error $ intersperse "" (map show errors'')
        throwError $ ExitFailure 1

    return program''
    where
        context = narrowContext baseContext (simp program)

-- | Type checks the program with the given narrow context.
--   Returns nothing if successful or yields the error message(s).
typeCheck :: Context -> Program SourcePos -> WorkerT IO ()
typeCheck context program = do
    let errors = execCheck (checkProgram program) context

    unless (null errors) $ do
        report Error $ intersperse "" (map show errors)
        throwError $ ExitFailure 1

-- | Compiles the program with the given narrow context to LLVM assembly.
--   Writes the compiled program to the given path.
compileToLlvmAssembly :: Context -> FilePath -> Program SourcePos -> WorkerT IO ()
compileToLlvmAssembly context binDir program = liftIO $ do
    assembly <- withContext $ \context ->
        runExceptT (withModuleFromAST context llvmModule moduleLLVMAssembly) >>= either fail return

    createDirectoryIfMissing True (takeDirectory filePath)
    writeFile filePath assembly
    where
        module_     = let (Program _ module_ _) = program in map simp module_
        llvmModule  = runReader (Compiler.compileProgram $ simp program) context
        filePath    = binDir </> intercalate [pathSeparator] module_ <.> "ll"

-- | Compiles the program with the given narrow context to LLVM bitcode.
--   Writes the compiled program to the given path.
compileToLlvmBitcode :: Context -> FilePath -> Program SourcePos -> WorkerT IO ()
compileToLlvmBitcode context binDir program = liftIO $ do
    bitcode <- withContext $ \context ->
        runExceptT (withModuleFromAST context llvmModule moduleBitcode) >>= either fail return

    createDirectoryIfMissing True (takeDirectory filePath)
    BS.writeFile filePath bitcode
    where
        module_     = let (Program _ module_ _) = program in map simp module_
        llvmModule  = runReader (Compiler.compileProgram $ simp program) context
        filePath    = binDir </> intercalate [pathSeparator] module_ <.> "bc"
