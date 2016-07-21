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
import              Data.Function   (on)
import              Data.List.Extra (groupBy, intercalate, intersperse, lower, sortOn)

import qualified    Language.Qux.Annotated.NameResolver     as NameResolver
import              Language.Qux.Annotated.Parser           hiding (parse)
import qualified    Language.Qux.Annotated.Parser           as Parser
import              Language.Qux.Annotated.Syntax
import              Language.Qux.Annotated.TypeChecker
import qualified    Language.Qux.Annotated.TypeResolver     as TypeResolver
import qualified    Language.Qux.Llvm.Compiler              as Compiler

import LLVM.General
import LLVM.General.Context hiding (Context)

import Prelude hiding (log)

import Qux.Exception
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
    deriving (Eq, Show)

ext :: Format -> String
ext Assembly    = "ll"
ext Bitcode     = "bc"


-- | Builds the files according to the options.
handle :: Options -> WorkerT IO ()
handle options = do
    log Debug "Parsing programs ..."
    programs <- parseAll $ argFilePaths options

    libraryFilePaths <- concat <$> forM (optLibdirs options) (\libdir ->
        ifM (liftIO $ doesDirectoryExist libdir)
            (liftIO $ listFilesRecursive libdir)
            (log Warn (unwords ["Directory", libdir, "in libpath does not exist"]) >> return []))

    log Debug "Parsing libraries ..."
    libraries <- parseAll $ filter ((== ".qux") . takeExtension) libraryFilePaths

    build options programs libraries


build :: Options -> [Program SourcePos] -> [Program SourcePos] -> WorkerT IO ()
build options programs libraries = do
    log Debug "Applyng sanity checker ..."
    let modules = sortOn snd [(ann $ head id, map simp id) | (Program _ id _) <- programs ++ libraries]
    let duplicateModules = concat $ filter ((> 1) . length) (groupBy ((==) `on` snd) modules)

    unless (null duplicateModules) $ do
        report Error $ map (show . uncurry DuplicateModuleName) duplicateModules
        throwError $ ExitFailure 1

    log Debug "Applying name and type resolvers ..."
    programs' <- mapM (resolve baseContext') programs

    when (optTypeCheck options) $ do
        log Debug "Applying type checker ..."
        forM_ programs' $ \program -> typeCheck (context program) program

    when (optCompile options)   $ do
        log Debug "Compiling programs ..."

        let count = length programs'
        forM_ (zip [1..count] programs') $ \(index, program) -> do
            let module_ = let (Program _ module_ _) = program in module_
            let format  = optFormat options
            let binDir  = optDestination options

            log Debug $ unwords [
                "[" ++ show index, "of", show count ++ "]",
                "Compiling", simp $ qualify module_,
                "(->", binDir </> intercalate [pathSeparator] (map simp module_) <.> ext format ++ ")"
                ]

            compile (context program) format binDir program
    where
        baseContext'    = baseContext $ map simp (programs ++ libraries)
        context         = narrowContext baseContext' . simp

typeCheck :: Context -> Program SourcePos -> WorkerT IO ()
typeCheck context program = do
    let errors = execCheck (checkProgram program) context

    unless (null errors) $ do
        report Error $ intersperse "" (map show errors)
        throwError $ ExitFailure 1

compile :: Context -> Format -> FilePath -> Program SourcePos -> WorkerT IO ()
compile context format binDir program
    | format == Assembly    = liftIO $ do
        assembly <- withContext $ \context ->
            runExceptT (withModuleFromAST context llvmModule moduleLLVMAssembly) >>= either fail return

        createDirectoryIfMissing True (takeDirectory filePath)
        writeFile filePath assembly
    | format == Bitcode     = liftIO $ do
        bitcode <- withContext $ \context ->
            runExceptT (withModuleFromAST context llvmModule moduleBitcode) >>= either fail return

        createDirectoryIfMissing True (takeDirectory filePath)
        BS.writeFile filePath bitcode
    | otherwise             = error $ "internal error: format not implemented `" ++ lower (show format) ++ "'"
    where
        module_     = let (Program _ module_ _) = program in map simp module_
        llvmModule  = runReader (Compiler.compileProgram $ simp program) context
        filePath    = binDir </> intercalate [pathSeparator] module_ <.> ext format


-- Helper methods

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
