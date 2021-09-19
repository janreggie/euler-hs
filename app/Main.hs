module Main where

import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    strOption,
    value,
    (<**>),
  )
import Solver (solver)

data Parameters = Parameters
  { problemNumber :: Integer,
    filename :: String
  }

parameters :: Parser Parameters
parameters =
  Parameters
    <$> option
      auto
      ( long "problem"
          <> help "Problem number to solve"
          <> short 'p'
          <> metavar "X"
      )
    <*> strOption
      ( long "input"
          <> help "Input file"
          <> short 'i'
          <> metavar "FILE"
          <> value ""
      )

main :: IO ()
main = interpretParams =<< execParser opts
  where
    opts =
      info
        (parameters <**> helper)
        ( fullDesc
            <> progDesc "Solve problem X using input file FILE"
            <> header "euler-hs - Project Euler solutions in Haskell"
        )

interpretParams :: Parameters -> IO ()
interpretParams (Parameters x "") = print (solver x "")
interpretParams (Parameters x filename) = do
  s <- readFile filename
  print (solver x s)
