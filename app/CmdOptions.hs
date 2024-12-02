module CmdOptions (parse, AdventureOptions(AdventureOptions), showHelp) where

import           Options.Applicative (ParseError (ShowHelpText), Parser,
                                      ParserInfo, defaultPrefs, execParser,
                                      fullDesc, header, help, helper, info,
                                      infoOption, long, metavar, parserFailure,
                                      progDesc, progDescDoc, renderFailure,
                                      short, strOption, (<**>))
import           Prettyprinter       (Pretty (pretty), indent, vsep)

newtype AdventureOptions = AdventureOptions String

choice :: Parser AdventureOptions
choice = AdventureOptions
      <$> strOption
          ( long "adventure"
          <> short 'a'
          <> metavar "NAME"
          <> help "Name of adventure to load." )

versionOption :: String ->  Parser (a -> a)
versionOption s = infoOption s (long "version" <> help "Show version")

parse :: String -> IO AdventureOptions
parse = execParser . opts

-- Todo: Examine this suggestion from Claude a little more closely.
-- I don't like how it seems to subvert the normal path; there may be a better choice by
-- following a more idiomatic use of Options.Applicative inside of Main.hs.
showHelp :: String -> IO ()
showHelp s = do
    let parser = opts s
    let failure = parserFailure defaultPrefs parser (ShowHelpText Nothing) []
    let (help_txt, _exit_code) = renderFailure failure "TextAdventure-exe"
    putStr help_txt

opts :: String -> ParserInfo AdventureOptions
opts s = info (choice <**> versionOption s <**> helper)
    ( fullDesc
    <> progDesc "Run the named text adventure."
    <> header "Haskell Adventure - a journey into fun!"
    <> (progDescDoc . Just $ indent 2 $ vsep [pretty s])
    )
