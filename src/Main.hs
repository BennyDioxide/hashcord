module Main where

import Calamity hiding (token)
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Di qualified
import DiPolysemy
import GHC.Generics (Generic)
import Optics
import Polysemy qualified as P
import System.Directory
import Toml qualified
import Toml.Schema

data Config = Config
  { token :: Text,
    guildId :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

main :: IO ()
main = do
  Success _ Config {token} <- getXdgDirectory XdgConfig "hashcord/config.toml" >>= TIO.readFile <&> Toml.decode
  -- let Config {token, guildId} = config
  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      -- . usePrefix
      . runBotIO (BotToken token) defaultIntents
      $ do
        info @Text "Setup"
