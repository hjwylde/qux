
{-|
Module      : Qux.Commands.Dependencies

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Dependencies where

import Control.Monad.Except

import Data.Char    (isSpace)
import Data.Graph
import Data.List    ((\\), intercalate)
import Data.Tree

import              Language.Qux.Annotated.Parser
import qualified    Language.Qux.Annotated.Syntax as Ann
import              Language.Qux.Syntax

import qualified Qux.Commands.Build as Build

import System.Exit
import System.IO
import System.IO.Error


data Options = Options {
    argFilePaths    :: [FilePath]
    }
    deriving (Eq, Show)


handle :: Options -> IO ()
handle options = do
    let filePaths = argFilePaths options
    fileContents <- mapM readFile filePaths

    ethr <- catchIOError
        (runExceptT $ zipWithM Build.parse filePaths fileContents >>= dependencies options)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

dependencies :: Options -> [Ann.Program SourcePos] -> ExceptT String IO ()
dependencies _ programs = do
    let cycles' = cycles graph

    when (not $ null cycles') $ throwError (concat [
        "error: dependency cycle",
        if length cycles' > 1 then "s" else "",
        " detected",
        foldMap (("\n" ++) . intercalate " <-> " . flatten . replace) cycles'
        ])

    liftIO $ putStrLn (trim . drawForest $ map replace (dependencyTree graph))
    where
        (graph, lookup, _)  = dependencyGraph programs
        replace             = fmap (\vertex -> let (_, id, _) = lookup vertex in intercalate "." id)
        trim                = reverse . dropWhile isSpace . reverse

dependencyGraph :: [Ann.Program SourcePos] -> (Graph, Vertex -> (Ann.Program SourcePos, [Id], [[Id]]), [Id] -> Maybe Vertex)
dependencyGraph programs = graphFromEdges [(p, map Ann.simp module_, imports decls) | p@(Ann.Program _ module_ decls) <- programs]
    where
        imports decls = [map Ann.simp id | (Ann.ImportDecl _ id) <- decls]

dependencyTree :: Graph -> Forest Vertex
dependencyTree graph = dependencyTree' graph (vertices graph \\ map snd (edges graph))
    where
        dependencyTree' graph roots = [Node {
            rootLabel = root,
            subForest = dependencyTree' graph (reachable graph root \\ [root])
            } | root <- roots]

cycles :: Graph -> Forest Vertex
cycles graph = filter (\tree -> not . null $ subForest tree) (scc graph)

