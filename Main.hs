module Main where

import Options.Applicative

data Greet = Greet { hello :: String, quiet :: Bool}

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

greetParser :: Parser Greet
greetParser = Greet
  <$> strOption
    ( long "hello"
    <> metavar "TARGET"
    <> help "Target for the greeting")
  <*> switch
    ( long "quite"
    <> help "Whether to be quiet")

main :: IO ()
main = do
  greet <- execParser $ info greetParser mempty
  case greet of
    Greet h False -> putStrLn $ "Hello, " ++ h
    _             -> return ()
