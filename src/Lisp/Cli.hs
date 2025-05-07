module Lisp.Cli (
  run
  ) where

import Options.Applicative
import Lisp.Repl

data Option = Repl | LoadThenRepl String

loadThenReplP :: Parser Option
loadThenReplP
  = LoadThenRepl <$> strOption
    (long "load" <> metavar "FILE" <> help "Load FILE then run the repl")

replP :: Parser Option
replP= flag' Repl $ long "repl" <> help "Run the repl"

optionP :: Parser Option
optionP = loadThenReplP <|> replP

getOption :: IO Option
getOption = execParser $ info (optionP <**> helper) $
  fullDesc <> progDesc "LISP interpreter, meritamen<meritamen@sdf.org>"

runOption :: Option -> IO ()
runOption Repl                    = runRepl Nothing
runOption (LoadThenRepl fileName) = runRepl $ Just fileName

run :: IO ()
run = getOption >>= runOption
