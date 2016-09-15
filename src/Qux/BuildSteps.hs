{-|
Module      : Qux.BuildSteps
Description : Build step utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Build step utilities.
-}

module Qux.BuildSteps (
    -- Expanding library directories
    expandLibdir, expandLibdirs,

    -- * Parsing
    parse, parseAll,

    -- * Sanity checking
    sanityCheckAll,

    -- * Resolving
    resolve, resolveAll,

    -- * Type checking
    typeCheck, typeCheckAll,

    -- * Compiling
    compileToLlvmAssembly, compileAllToLlvmAssembly, compileToLlvmBitcode, compileAllToLlvmBitcode,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader

import qualified Data.ByteString as BS
import           Data.Function
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

import Qux.Exception
import Qux.Worker

import System.Directory.Extra
import System.Exit
import System.FilePath

expandLibdir :: FilePath -> WorkerT IO [FilePath]
expandLibdir libdir = do
    libraryFilePaths <- ifM (liftIO $ doesDirectoryExist libdir) listFilesRecursive' (logWarn >> return [])

    return $ filter ((== ".qux") . takeExtension) libraryFilePaths
    where
        listFilesRecursive' = liftIO $ listFilesRecursive libdir
        logWarn             = log Warn (unwords ["Directory", libdir, "in libpath does not exist"])

expandLibdirs :: [FilePath] -> WorkerT IO [FilePath]
expandLibdirs libdirs = concat <$> mapM expandLibdir libdirs

-- | Parses the file. Returns the program if successful or yields the error message(s).
parse :: FilePath -> WorkerT IO (Program SourcePos)
parse filePath = do
    unlessM (liftIO $ doesFileExist filePath) $ do
        log Error $ "Cannot find file " ++ filePath
        throwError $ ExitFailure 1

    contents <- liftIO $ readFile filePath

    case runExcept (Parser.parse program filePath contents) of
        Left error      -> log Error (show error) >> throwError (ExitFailure 1)
        Right program   -> return program

-- | Parses the files. Returns the programs if successful or yields the error message(s).
parseAll :: [FilePath] -> WorkerT IO [Program SourcePos]
parseAll = mapM parse

-- | Sanity checks the programs and libraries. The checks comprise of looking for duplicate modules
--   (both programs and libraries combined). Returns nothing if successful or yields the error
--   message(s).
sanityCheckAll :: [Program SourcePos] -> [Program SourcePos] -> WorkerT IO ()
sanityCheckAll programs libraries = unless (null duplicateModules) $ do
    report Error (map (show . uncurry DuplicateModuleName) duplicateModules)
    throwError $ ExitFailure 1
    where
        modules             = sortOn snd
            [ (ann $ head id, map simp id)
            | (Program _ id _) <- programs ++ libraries
            ]
        duplicateModules    = concat $ filter ((> 1) . length) (groupBy ((==) `on` snd) modules)

-- | Resolves the program with the given base context. Returns the program if successful or yields
--   the error message(s).
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

-- | Resolves all the programs with the given base context. Returns the programs if successful or
--   yields the error message(s).
resolveAll :: Context -> [Program SourcePos] -> WorkerT IO [Program SourcePos]
resolveAll baseContext = mapM (resolve baseContext)

-- | Type checks the program with the given narrow context. Returns nothing if successful or yields
--   the error message(s).
typeCheck :: Context -> Program SourcePos -> WorkerT IO ()
typeCheck context program = do
    let errors = execCheck (checkProgram program) context

    unless (null errors) $ do
        report Error $ intersperse "" (map show errors)
        throwError $ ExitFailure 1

-- | Type checks the programs with the given base context. Returns nothing if successful or yields
--   the error message(s).
typeCheckAll :: Context -> [Program SourcePos] -> WorkerT IO ()
typeCheckAll baseContext programs = forM_ programs typeCheck'
    where
        typeCheck' program  = typeCheck (context program) program
        context             = narrowContext baseContext . simp

-- | Compiles the program with the given narrow context to LLVM assembly. Writes the compiled
--   program to the appropriate path appended to the given bin directory.
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

-- | Compiles all the programs with the given base context to LLVM assembly. Writes the compiled
--   programs to their appropriate path appended to the given bin directory.
compileAllToLlvmAssembly :: Context -> FilePath -> [Program SourcePos] -> WorkerT IO ()
compileAllToLlvmAssembly baseContext binDir programs = forM_ indexedPrograms $ \(index, program) -> do
    let module_ = let (Program _ module_ _) = program in module_
    let context = narrowContext baseContext (simp program)

    log Debug $ unwords
        [ "[" ++ show index, "of", show count ++ "]"
        , "Compiling", simp $ qualify module_
        , "(->", binDir </> intercalate [pathSeparator] (map simp module_) <.> "ll" ++ ")"
        ]

    compileToLlvmAssembly context binDir program
    where
        count           = length programs
        indexedPrograms = zip [1..count] programs

-- | Compiles the program with the given narrow context to LLVM bitcode. Writes the compiled program
--   to the appropriate path appended to the given bin directory.
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

-- | Compiles all the programs with the given base context to LLVM bitcode. Writes the compiled
--   programs to their appropriate path appended to the given bin directory.
compileAllToLlvmBitcode :: Context -> FilePath -> [Program SourcePos] -> WorkerT IO ()
compileAllToLlvmBitcode baseContext binDir programs = forM_ indexedPrograms $ \(index, program) -> do
    let module_ = let (Program _ module_ _) = program in module_
    let context = narrowContext baseContext (simp program)

    log Debug $ unwords
        [ "[" ++ show index, "of", show count ++ "]"
        , "Compiling", simp $ qualify module_
        , "(->", binDir </> intercalate [pathSeparator] (map simp module_) <.> "bc" ++ ")"
        ]

    compileToLlvmBitcode context binDir program
    where
        count           = length programs
        indexedPrograms = zip [1..count] programs
