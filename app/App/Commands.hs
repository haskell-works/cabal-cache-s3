module App.Commands where

import App.Commands.Get
import App.Commands.Head
import App.Commands.Put
import App.Commands.Version
import Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdGet
  <>  cmdHead
  <>  cmdPut
  <>  cmdVersion
