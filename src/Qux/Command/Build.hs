{-|
Module      : Qux.Command.Build
Description : Options and handler for the build subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the build subcommand.
-}

module Qux.Command.Build (
    -- * Options
    Options(..),
    defaultOptions,

    Format(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except

import Data.Function
import Data.List.Extra

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax
import Language.Qux.Annotated.TypeChecker

import Prelude hiding (log)

import qualified Qux.BuildSteps as BuildSteps
import           Qux.Worker

import System.FilePath

-- | Build options.
data Options = Options
    { optCompile     :: Bool        -- ^ Flag for compiling to LLVM.
    , optDestination :: FilePath    -- ^ The destination folder to write the compiled files.
    , optFormat      :: Format      -- ^ The output format.
    , optLibdirs     :: [FilePath]  -- ^ Directories to search for extra library files to reference (but not to compile).
    , optTypeCheck   :: Bool        -- ^ Flag for type checking the files.
    , argFilePaths   :: [FilePath]  -- ^ The files to compile.
    } deriving (Eq, Show)

-- | The default build options.
defaultOptions :: Options
defaultOptions = Options
    { optCompile        = False
    , optDestination    = "."
    , optFormat         = Bitcode
    , optLibdirs        = []
    , optTypeCheck      = False
    , argFilePaths      = []
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
    programs <- BuildSteps.parseAll $ argFilePaths options

    log Debug "Parsing libraries ..."
    libraries <- BuildSteps.parseAll =<< BuildSteps.expandLibdirs (optLibdirs options)

    build options programs libraries

build :: Options -> [Program SourcePos] -> [Program SourcePos] -> WorkerT IO ()
build options programs libraries = do
    log Debug "Applying sanity checker ..."
    BuildSteps.sanityCheckAll programs libraries

    log Debug "Applying name and type resolvers ..."
    programs' <- BuildSteps.resolveAll baseContext' programs

    when (optTypeCheck options) $ do
        log Debug "Applying type checker ..."
        BuildSteps.typeCheckAll baseContext' programs'

    when (optCompile options) $ do
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

            case format of
                Assembly    -> BuildSteps.compileToLlvmAssembly (context program) binDir program
                Bitcode     -> BuildSteps.compileToLlvmBitcode (context program) binDir program
    where
        baseContext'    = baseContext $ map simp (programs ++ libraries)
        context         = narrowContext baseContext' . simp
