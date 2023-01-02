module Config (Config (..), readConfig) where

import Data.Text (Text)
import Env

data Config = Config
  { configTest :: Bool
  , configHost :: Text
  , configPort :: Integer
  }

readConfig :: IO Config
readConfig =
  Env.parse (header "envparse example") $
    Config <$> switch "MS_TEST" (help "When tru just use a test video pattern instead of the camera")
    <*> var str "MS_HOST"  (def "0.0.0.0" <> helpDef show <> help "Network interface for streaming")
    <*> var auto "MS_PORT"  (def 5000 <> helpDef show <> help "Network port for streaming")
