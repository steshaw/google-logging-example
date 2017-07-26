{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~), (<&>), (?~))
import Control.Exception
import Data.Text (Text)
import Network.Google as Google
import Network.Google.Logging
import System.IO (stdout)

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

import qualified Data.Text as T
import qualified System.Environment as Env

pprint :: Show a => a -> IO ()
pprint =
  putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

getEnv :: IO String
getEnv = do
  -- manager <- newManager tlsManagerSettings
  -- FIXME: Need to ask the environment what our project-id etc is for logging.
  --
  -- Name of log should be something like:
  --   projects/my-project-137423/logs/my-app
  --
  -- MonitoredResource should contain:
  --   labels: {
  --     cluster_name:  "cluster-1"
  --     container_name:  "my-app"
  --     instance_id:  "7937682100289506354"
  --     namespace_id:  "default"
  --     pod_id:  "my-app-3144516956-msvv1"
  --     project_id:  "my-project-137423"
  --     zone:  "us-central1-a"
  --   }
  let resource = "a-resource"
  return resource

logMsg :: Text -> Text -> IO (Rs EntriesWrite)
logMsg logName msg = do
  lgr <- newLogger Google.Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ loggingWriteScope)

  let
    entry = logEntry & leTextPayload ?~ msg
    entries = [entry]
    resource = monitoredResource & mrType ?~ "container"

  runResourceT . runGoogle env $
    send
      (entriesWrite
        (writeLogEntriesRequest
          & wlerEntries .~ entries
          & wlerLogName ?~ logName
          & wlerResource ?~ resource
--        & wlerLabels .~ labels
        )
      )

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [logName, msg] ->
      handle (\e -> pprint ("exception" :: Text, e :: SomeException)) $ do
        putStrLn "Writing log message..."
        result <- logMsg (T.pack logName) (T.pack msg)
        pprint ("result" :: Text, result)
    _ -> putStrLn "usage: google-log log-name message"
