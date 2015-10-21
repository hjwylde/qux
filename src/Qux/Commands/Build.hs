
{-|
Module      : Qux.Commands.Build
Description : Options and handler for the build subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the build subcommand.
-}

module Qux.Commands.Build (
    -- * Options
    Options(..),
    defaultOptions,

    Format(..),

    -- * Handle
    handle,

    -- * Utility
    parse, parseAll,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader

import qualified    Data.ByteString as BS
import              Data.List       (intercalate, intersperse)

import qualified    Language.Qux.Annotated.NameResolver     as NameResolver
import              Language.Qux.Annotated.Parser           hiding (parse)
import qualified    Language.Qux.Annotated.Parser           as P
import              Language.Qux.Annotated.Syntax
import              Language.Qux.Annotated.TypeChecker
import qualified    Language.Qux.Annotated.TypeResolver     as TypeResolver
import qualified    Language.Qux.Llvm.Compiler              as C

import LLVM.General
import LLVM.General.Context hiding (Context)

import Pipes

import Qux.Worker

import System.Directory.Extra
import System.Exit
import System.FilePath


-- | Build options.
data Options = Options {
    optCompile      :: Bool,        -- ^ Flag for compiling to LLVM.
    optDestination  :: FilePath,    -- ^ The destination folder to write the compiled files.
    optFormat       :: Format,      -- ^ The output format.
    optLibdirs      :: [FilePath],  -- ^ Directories to search for extra library files to reference (but not to compile).
    optTypeCheck    :: Bool,        -- ^ Flag for type checking the files.
    argFilePaths    :: [FilePath]   -- ^ The files to compile.
    }
    deriving (Eq, Show)

-- | The default build options.
defaultOptions :: Options
defaultOptions = Options {
    optCompile      = False,
    optDestination  = ".",
    optFormat       = Bitcode,
    optLibdirs      = [],
    optTypeCheck    = False,
    argFilePaths    = []
    }


-- | Output format for the compiled LLVM code.
data Format = Assembly | Bitcode
    deriving Eq

instance Show Format where
    show Assembly   = "assembly"
    show Bitcode    = "bitcode"


-- | Builds the files according to the options.
handle :: Options -> IO ()
handle options = runWorkerT $ do
    libraryFilePaths <- liftIO $ concat <$> mapM
        (\libdir ->
            ifM (doesDirectoryExist libdir) (listFilesRecursive libdir) (return []))
        (optLibdirs options)

    programs    <- parseAll $ argFilePaths options
    libraries   <- parseAll $ filter ((== ".qux") . takeExtension) libraryFilePaths

    build options programs libraries


build :: Options -> [Program SourcePos] -> [Program SourcePos] -> WorkerT IO ()
build options programs libraries = do
    programs' <- mapM (resolve baseContext') programs

    when (optTypeCheck options) $ mapM_ (\program -> typeCheck (pContext program) program) programs'
    when (optCompile options)   $ mapM_ (\program -> compile options (pContext program) program) programs'
    where
        baseContext'    = baseContext $ map simp (programs ++ libraries)
        pContext        = narrowContext baseContext' . simp

typeCheck :: Context -> Program SourcePos -> WorkerT IO ()
typeCheck context program = do
    let errors = execCheck (checkProgram program) context

    when (not $ null errors) $ do
        each $ intersperse "" (map show errors)
        throwError $ ExitFailure 1

compile :: Options -> Context -> Program SourcePos -> WorkerT IO ()
compile options context program
    | optFormat options == Assembly = liftIO $ do
        assembly <- withContext $ \context ->
            runExceptT (withModuleFromAST context mod moduleLLVMAssembly) >>= either fail return

        createDirectoryIfMissing True basePath
        writeFile (basePath </> baseName <.> "ll") assembly
    | optFormat options == Bitcode  = liftIO $ do
        bitcode <- withContext $ \context ->
            runExceptT (withModuleFromAST context mod moduleBitcode) >>= either fail return

        createDirectoryIfMissing True basePath
        BS.writeFile (basePath </> baseName <.> "bc") bitcode
    | otherwise                     = error $ "internal error: format not implemented `" ++ show (optFormat options) ++ "'"
    where
        module_     = let (Program _ module_ _) = program in map simp module_
        mod         = runReader (C.compileProgram $ simp program) context
        basePath    = intercalate [pathSeparator] (optDestination options:init module_)
        baseName    = last module_


-- Helper methods

-- | Parses the file.
--   Returns the program if successful or yields the error message(s).
parse :: FilePath -> WorkerT IO (Program SourcePos)
parse filePath = do
    whenM (not <$> liftIO (doesFileExist filePath)) $ do
        yield $ "Cannot find file " ++ filePath
        throwError $ ExitFailure 1

    contents <- liftIO $ readFile filePath

    case runExcept (P.parse program filePath contents) of
        Left error      -> yield (show error) >> throwError (ExitFailure 1)
        Right program   -> return program

-- | Parses the files.
--   Returns the programs if successful or yields the error message(s).
parseAll :: [FilePath] -> WorkerT IO [Program SourcePos]
parseAll = mapM parse

resolve :: Context -> Program SourcePos -> WorkerT IO (Program SourcePos)
resolve baseContext program = do
    let (program', errors') = NameResolver.runResolve (NameResolver.resolveProgram program) context
    when (not $ null errors') $ do
        each $ intersperse "" (map show errors')
        throwError $ ExitFailure 1

    let (program'', errors'') = TypeResolver.runResolve (TypeResolver.resolveProgram program') context
    when (not $ null errors'') $ do
        each $ intersperse "" (map show errors'')
        throwError $ ExitFailure 1

    return program''
    where
        context = narrowContext baseContext (simp program)

