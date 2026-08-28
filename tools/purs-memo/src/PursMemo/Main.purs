-- | CLI entry point. Usage: `purs-memo [--no-prune] <path>`, where `<path>`
-- | is a `.purs` file or a directory searched recursively. Transforms each
-- | file in place. The path is always the last argument, so this tolerates
-- | whatever placeholder tokens precede it (e.g. `node -e '...' -- purs-memo
-- | <path>`, where `process.argv` also carries the node binary and the
-- | `purs-memo` placeholder ahead of the real argument).
module PursMemo.Main (main) where

import Prelude

import Data.Array (all, concat, elem, last, uncons)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
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
      files <- pursFilesUnder path
      results <- traverseEffect (transformFile opts) files
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
    nested <- traverseEffect (\e -> pursFilesUnder (path <> "/" <> e)) entries
    pure (concat nested)
  else
    pure (if String.contains (Pattern ".purs") path then [ path ] else [])

traverseEffect :: forall a b. (a -> Effect b) -> Array a -> Effect (Array b)
traverseEffect f = go []
  where
  go acc xs = case uncons xs of
    Nothing -> pure acc
    Just { head, tail } -> do
      b <- f head
      go (acc <> [ b ]) tail
