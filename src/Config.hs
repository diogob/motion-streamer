module Config (Config (..), readConfig) where

import Env

data Config = Config {configTest :: Bool}

readConfig :: IO Config
readConfig =
  Env.parse (header "envparse example") $
    Config <$> switch "TEST" (help "When tru just use a test video pattern instead of the camera")