{-|
Module      : Qux.Commands.Check
Description : Options and handler for the check subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the check subcommand.
-}

module Qux.Commands.Check (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import qualified    Qux.Commands.Build as Build
import              Qux.Worker


-- | Check options.
data Options = Options {
    argFilePaths :: [FilePath] -- ^ The files to type check.
    }
    deriving (Eq, Show)


-- | Calls 'Build.handle', passing it the default options with a flag to type check the files.
handle :: Options -> WorkerT IO ()
handle options = Build.handle $ buildOptions options


buildOptions :: Options -> Build.Options
buildOptions options = Build.defaultOptions {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }
