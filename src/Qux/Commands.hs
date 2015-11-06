
{-|
Module      : Qux.Commands
Description : Optparse utilities for the qux command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Optparse utilities for the qux command.
-}

module Qux.Commands (
    -- * Options
    Options(..), Command(..),

    -- * Optparse for Qux
    quxPrefs, quxInfo, qux,
) where

import Data.List.Extra  (lower, nub)
import Data.Version     (showVersion)

import qualified Language.Qux.Version as Qux

import Options.Applicative
import Options.Applicative.Types (readerAsk)

import Prelude hiding (print)

import qualified Qux.Commands.Build         as Build
import qualified Qux.Commands.Check         as Check
import qualified Qux.Commands.Compile       as Compile
import qualified Qux.Commands.Dependencies  as Dependencies
import qualified Qux.Commands.Print         as Print
import qualified Qux.Version                as This

import System.FilePath

import Text.PrettyPrint (Mode(..))


-- | Main options.
data Options = Options {
    optQuiet    :: Bool,    -- ^ Flag for quiet output.
    optVerbose  :: Bool,    -- ^ Flag for verbose output.
    argCommand  :: Command  -- ^ Command to run.
    }
    deriving (Eq, Show)

-- | A command and associated options.
data Command    = Build         Build.Options
                | Check         Check.Options
                | Compile       Compile.Options
                | Dependencies  Dependencies.Options
                | Print         Print.Options
    deriving (Eq, Show)


-- | The default preferences.
--   Limits the help output to 100 columns.
quxPrefs :: ParserPrefs
quxPrefs = prefs $ columns 100

-- | An optparse parser of a qux command.
quxInfo :: ParserInfo Options
quxInfo = info (infoOptions <*> qux) fullDesc
    where
        infoOptions = helper <*> version <*> numericVersion <*> quxVersion
        version = infoOption ("Version " ++ showVersion This.version) $ mconcat [
            long "version", short 'V', hidden,
            help "Show this binary's version"
            ]
        numericVersion = infoOption (showVersion This.version) $ mconcat [
            long "numeric-version", hidden,
            help "Show this binary's version (without the prefix)"
            ]
        quxVersion = infoOption ("Qux version " ++ showVersion Qux.version) $ mconcat [
            long "qux-version", hidden,
            help "Show the qux version this binary was compiled with"
            ]

-- | A command subparser.
qux :: Parser Options
qux = Options
    <$> switch (mconcat [
        long "quiet", short 'q',
        help "Be quiet (only show errors)"
        ])
    <*> switch (mconcat [
        long "verbose", short 'v',
        help "Be verbose (show all messages and timestamps)"
        ])
    <*> subparser (mconcat [
        command "build"         $ info (helper <*> build)           (fullDesc <> progDesc "Build FILES using composable options"),
        command "check"         $ info (helper <*> check)           (fullDesc <> progDesc "Check FILES for correctness" <> header "Shortcut for `qux build --type-check'"),
        command "compile"       $ info (helper <*> compile)         (fullDesc <> progDesc "Compile FILES into the LLVM IR" <> header "Shortcut for `qux build --compile --type-check'"),
        command "dependencies"  $ info (helper <*> dependencies)    (fullDesc <> progDesc "Print out the module dependencies of FILES"),
        command "print"         $ info (helper <*> print)           (fullDesc <> progDesc "Pretty print FILE")
        ])


build :: Parser Command
build = fmap Build $ Build.Options
    <$> switch (mconcat [
        long "compile", short 'c',
        help "Enable compilation to LLVM IR"
        ])
    <*> strOption (mconcat [
        long "destination", short 'd', metavar "DIR",
        value ("." ++ [pathSeparator]), showDefault,
        help "Specify the output directory to put the compiled LLVM IR"
        ])
    <*> formatOption (mconcat [
        long "format", short 'f', metavar "FORMAT",
        value Build.Bitcode, showDefaultWith $ \format -> lower (show format),
        help "Specify the LLVM output format as either `bitcode' or `assembly'"
        ])
    <*> fmap (\path -> if null path then [] else nub $ splitSearchPath path) (strOption $ mconcat [
        long "libpath", short 'l', metavar "PATH",
        value "",
        help "Specify a path separated list of where to try find referenced libraries"
        ])
    <*> switch (mconcat [
        long "type-check",
        help "Enable type checking"
        ])
    <*> fmap nub (some $ strArgument (mconcat [
        metavar "-- FILES..."
        ]))

check :: Parser Command
check = Check . Check.Options . nub
    <$> some (strArgument $ mconcat [
        metavar "FILES..."
        ])

compile :: Parser Command
compile = fmap Compile $ Compile.Options
    <$> strOption (mconcat [
        long "destination", short 'd', metavar "DIR",
        value ("." ++ [pathSeparator]), showDefault,
        help "Specify the output directory to put the compiled LLVM IR"
        ])
    <*> formatOption (mconcat [
        long "format", short 'f', metavar "FORMAT",
        value Build.Bitcode, showDefaultWith $ \format -> lower (show format),
        help "Specify the LLVM output format as either `bitcode' or `assembly'"
        ])
    <*> fmap (\path -> if null path then [] else nub $ splitSearchPath path) (strOption $ mconcat [
        long "libpath", short 'l', metavar "PATH",
        value "",
        help "Specify a path separated list of where to try find referenced libraries"
        ])
    <*> fmap nub (some $ strArgument (mconcat [
        metavar "-- FILES..."
        ]))

dependencies :: Parser Command
dependencies = Dependencies . Dependencies.Options . nub
    <$> some (strArgument $ mconcat [
        metavar "FILES..."
        ])

print :: Parser Command
print = fmap Print $ Print.Options
    <$> option auto (mconcat [
        long "line-length", short 'l', metavar "LENGTH",
        value 100, showDefault,
        help "Specify the maximum line length"
        ])
    <*> modeOption (mconcat [
        long "mode", short 'm', metavar "MODE",
        value PageMode, showDefaultWith $ const "normal",
        help "Specify the rendering mode as either `normal' or `one-line'"
        ])
    <*> option auto (mconcat [
        long "ribbons-per-line", short 'r', metavar "RIBBONS",
        value 1.5, showDefault,
        help "Specify the ratio of line length to ribbon length"
        ])
    <*> strArgument (mconcat [
        metavar "FILE"
        ])
    where
        modeOption = option $ readerAsk >>= \opt -> case opt of
            "normal"    -> return PageMode
            "one-line"  -> return OneLineMode
            _           -> readerError $ "unrecognised mode `" ++ opt ++ "'"


-- Helper methods

formatOption :: Mod OptionFields Build.Format -> Parser Build.Format
formatOption = option $ readerAsk >>= \opt -> case opt of
    "assembly"  -> return Build.Assembly
    "bitcode"   -> return Build.Bitcode
    _           -> readerError $ "unrecognised format `" ++ opt ++ "'"

