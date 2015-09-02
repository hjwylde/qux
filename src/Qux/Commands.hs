
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands where

import Data.Version (showVersion)

import Language.Qux.Annotated.PrettyPrinter
import qualified Language.Qux.Version as Qux

import Options.Applicative
import Options.Applicative.Types

import Prelude hiding (print)

import qualified Qux.Commands.Build as Build
import qualified Qux.Commands.Check as Check
import qualified Qux.Commands.Print as Print
import qualified Qux.Commands.Run   as Run
import qualified Qux.Version        as Binary


-- * Qux command

options :: ParserInfo Options
options = info (helper <*> version <*> quxVersion <*> qux) (fullDesc <> noIntersperse)
    where
        version = infoOption ("Version " ++ showVersion Binary.version) $ mconcat [
            long "version",
            short 'V',
            help "Show this binary's version",
            hidden
            ]
        quxVersion = infoOption ("Qux version " ++ showVersion Qux.version) $ mconcat [
            long "qux-version",
            help "Show the qux version this binary was compiled with",
            hidden
            ]

data Options = Options {
    argCommand :: Command
    }

-- TODO (hjw): show the help message if no commands given
-- TODO (hjw): improve error message to say "Invalid command x"
qux :: Parser Options
qux = Options <$> subparser (mconcat [
    command "build" $ info (helper <*> build)   (fullDesc <> progDesc "Build FILE using composable options"),
    command "check" $ info (helper <*> check)   (fullDesc <> progDesc "Check FILE for correctness" <> header "Shortcut for `qux build --type-check'"),
    command "print" $ info (helper <*> print)   (fullDesc <> progDesc "Pretty print FILE"),
    command "run"   $ info (helper <*> run)     (fullDesc <> progDesc "Run FILE passing in ARGS as program arguments")
    ])


-- * Subcommands

data Command    = Build Build.Options
                | Check Check.Options
                | Print Print.Options
                | Run   Run.Options


-- ** Build

build :: Parser Command
build = fmap Build $ Build.Options
    <$> switch (mconcat [
        long "type-check",
        help "Enable type checking"
        ])
    <*> strArgument (mconcat [
        metavar "FILE"
        ])


-- ** Check

check :: Parser Command
check = Check . Check.Options <$> strArgument (mconcat [
    metavar "FILE"
    ])


-- ** Print

print :: Parser Command
print = fmap Print $ Print.Options
    <$> option auto (mconcat [
        long "line-length",
        short 'l',
        metavar "LENGTH",
        value 100,
        showDefault,
        help "Specify the maximum line length"
        ])
    <*> modeOption (mconcat [
        long "mode",
        short 'm',
        metavar "MODE",
        value PageMode,
        showDefaultWith $ const "normal",
        help "Specify the rendering mode as either 'normal' or 'one-line'"
        ])
    <*> option auto (mconcat [
        long "ribbons-per-line",
        short 'r',
        metavar "RIBBONS",
        value 1.5,
        showDefault,
        help "Specify the ratio of line length to ribbon length"
        ])
    <*> strArgument (mconcat [
        metavar "FILE"
        ])
    where
        modeOption :: Mod OptionFields Mode -> Parser Mode
        modeOption = option $ do
            opt <- readerAsk

            case opt of
                "normal"    -> return PageMode
                "one-line"  -> return OneLineMode
                _           -> readerError $ "unrecognised mode `" ++ opt ++ "'"

-- ** Run

run :: Parser Command
run = fmap Run $ Run.Options
    <$> strOption (mconcat [
        long "entry",
        short 'e',
        metavar "NAME",
        value "main",
        showDefault,
        help "Specify the program entry point"
        ])
    <*> strArgument (mconcat [
        metavar "FILE"
        ])
    <*> many (strArgument $ mconcat [
        metavar "ARGS..."
        ])

