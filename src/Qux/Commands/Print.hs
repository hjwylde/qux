
{-|
Module      : Qux.Commands.Print

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Print where

import Control.Monad.Except

import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Annotated.Syntax

import Prelude hiding (print)

import qualified    Qux.Commands.Build as Build
import              Qux.Worker

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass


data Options = Options {
    optLineLength       :: Int,
    optMode             :: Mode,
    optRibbonsPerLine   :: Float,
    argFilePath         :: FilePath
    }
    deriving (Eq, Show)


handle :: Options -> IO ()
handle options = runWorkerT $ Build.parse (argFilePath options) >>= print options

print :: Options -> Program SourcePos -> WorkerT IO ()
print options program = liftIO $ putStrLn (renderStyle style (pPrint program))
    where
        style = Style {
            mode            = optMode options,
            lineLength      = optLineLength options,
            ribbonsPerLine  = optRibbonsPerLine options
            }

