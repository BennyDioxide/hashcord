module Main where

import Calamity hiding (count, emoji, token, user)
import Calamity.Cache.InMemory
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop
import Calamity.Types.Model.Channel.Reaction
import Control.Monad
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Di qualified
import DiPolysemy
import GHC.Generics (Generic)
import Optics
import Polysemy qualified as P
import System.Directory
import TextShow
import Toml qualified
import Toml.Schema

data Config = Config
  { token :: Text,
    defaultStarCount :: Integer
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

main :: IO ()
main = do
  Success _ Config {token, defaultStarCount} <- getXdgDirectory XdgConfig "hashcord/config.toml" >>= TIO.readFile <&> Toml.decode
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

        react @'MessageReactionAddEvt $ \(message, user, channel, _emoji) -> do
          debug $ showt message
          let stars = filter (\r -> emoji r == UnicodeEmoji "⭐" && count r >= defaultStarCount) $ reactions message
          case stars of
            stars' : _ -> info @Text $ "User " <> showt user <> " in channel " <> showt channel <> " has got " <> showt (count stars') <> " stars"
            _ -> return ()
