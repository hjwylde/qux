{-|
Module      : Qux.Command.Compile
Description : Options and handler for the compile subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the compile subcommand.
-}

module Qux.Command.Compile (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import qualified Qux.Command.Build as Build
import           Qux.Worker

-- | Compile options.
data Options = Options
    { optDestination :: FilePath        -- ^ The destination folder to write the compiled files.
    , optFormat      :: Build.Format    -- ^ The output format.
    , optLibdirs     :: [FilePath]      -- ^ Directories to search for extra library files to reference (but not to compile).
    , argFilePaths   :: [FilePath]      -- ^ The files to compile.
    } deriving (Eq, Show)

-- | Calls 'Build.handle', passing it the default options along with the compile options.
--   This also sets the flag to type check the files.
handle :: Options -> WorkerT IO ()
handle options = Build.handle $ buildOptions options

buildOptions :: Options -> Build.Options
buildOptions options = Build.defaultOptions
    { Build.optCompile      = True
    , Build.optDestination  = optDestination options
    , Build.optFormat       = optFormat options
    , Build.optLibdirs      = optLibdirs options
    , Build.optTypeCheck    = True
    , Build.argFilePaths    = argFilePaths options
    }
