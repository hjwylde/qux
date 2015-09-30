
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Print

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Print where

import Control.Monad.Except

import Language.Qux.Annotated.PrettyPrinter

import Qux.Commands.Build (parse)

import System.Exit
import System.IO
import System.IO.Error


data Options = Options {
    optLineLength       :: Int,
    optMode             :: Mode,
    optRibbonsPerLine   :: Float,
    argFilePath         :: FilePath
    }

handle :: Options -> IO ()
handle options = do
    let filePath = argFilePath options
    contents <- readFile $ argFilePath options

    ethr <- catchIOError
        (runExceptT $ parse filePath contents)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error      -> hPutStrLn stderr (show error) >> exitFailure
        Right program   -> putStrLn $ renderStyle (style options) (pPrint program)

style :: Options -> Style
style options = Style {
    mode            = optMode options,
    lineLength      = optLineLength options,
    ribbonsPerLine  = optRibbonsPerLine options
    }

