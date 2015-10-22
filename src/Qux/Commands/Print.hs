
{-|
Module      : Qux.Commands.Print
Description : Options and handler for the print subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the print subcommand.
-}

module Qux.Commands.Print (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Except

import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Annotated.Syntax

import Prelude hiding (log, print)

import qualified    Qux.Commands.Build as Build
import              Qux.Worker

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass


-- | Print options.
data Options = Options {
    optLineLength       :: Int,     -- ^ The maximum line length.
    optMode             :: Mode,    -- ^ The printing mode.
    optRibbonsPerLine   :: Float,   -- ^ The number of ribbons per line.
    argFilePath         :: FilePath -- ^ The file to pretty print.
    }
    deriving (Eq, Show)


-- | Pretty prints the file according to the options.
handle :: Options -> WorkerT IO ()
handle options = Build.parse (argFilePath options) >>= print options


print :: Options -> Program SourcePos -> WorkerT IO ()
print options program = log Info $ renderStyle style (pPrint program)
    where
        style = Style {
            mode            = optMode options,
            lineLength      = optLineLength options,
            ribbonsPerLine  = optRibbonsPerLine options
            }

