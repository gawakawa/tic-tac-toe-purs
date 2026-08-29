-- | CLI entry point. Usage: `purs-memo [--no-prune] <path>`; transforms each `.purs` file in place.
module PursMemo.Main (main) where

import Prelude

import Data.Array (all, concat, elem, last)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readTextFile, readdir, stat, writeTextFile)
import Node.Process (argv, exit')
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PursMemo.Transform (defaultOptions, transformModule)
import Tidy.Codegen (printModule)

main :: Effect Unit
main = do
  args <- argv
  case last args of
    Nothing -> usage
    Just path -> do
      let opts = { prune: not (elem "--no-prune" args) }
      -- A bad path (including a zero-arg run via the shipped wrapper) shows usage instead of crashing.
      catchException (\_ -> usage) do
        files <- pursFilesUnder path
        results <- traverse (transformFile opts) files
        if all identity results then pure unit else exit' 1

usage :: Effect Unit
usage = do
  log "usage: purs-memo [--no-prune] <path>"
  exit' 1

transformFile :: { prune :: Boolean } -> String -> Effect Boolean
transformFile opts path = do
  src <- readTextFile UTF8 path
  case parseModule src of
    ParseSucceeded m -> do
      writeTextFile UTF8 path
        ( printModule
            (transformModule (defaultOptions { prune = opts.prune }) m)
        )
      pure true
    ParseSucceededWithErrors _ _ -> do
      log ("purs-memo: parse errors in " <> path <> ", left unchanged")
      pure false
    ParseFailed _ -> do
      log ("purs-memo: could not parse " <> path <> ", left unchanged")
      pure false

pursFilesUnder :: String -> Effect (Array String)
pursFilesUnder path = do
  s <- stat path
  if isDirectory s then do
    entries <- readdir path
    nested <- traverse (\e -> pursFilesUnder (path <> "/" <> e)) entries
    pure (concat nested)
  else
    pure
      ( if isJust (String.stripSuffix (Pattern ".purs") path) then [ path ]
        else []
      )
