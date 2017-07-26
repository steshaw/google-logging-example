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

import qualified Data.HashMap.Lazy as HM
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
  -- MonitoredResource should contain to simulate Google's fluentd:
  --   labels: {
  --     cluster_name:  "cluster-1"
  --     container_name:  "my-app"
  --     instance_id:  "7937682100289506354"
  --     namespace_id:  "default"
  --     pod_id:  "my-app-3144516956-msvv1"
  --     project_id:  "my-project-137423"
  --     zone:  "us-central1-a"
  --   }
  --
  -- Entry labels should look something like:
  --
  -- labels: {
  --   compute.googleapis.com/resource_id:  "7937682100289506354"
  --   compute.googleapis.com/resource_name:  "fluentd-cloud-logging-gke-my-cluster-1-default-pool-dabd30fd-wv"
  --   compute.googleapis.com/resource_type:  "instance"
  --   container.googleapis.com/cluster_name:  "my-cluster-1"
  --   container.googleapis.com/container_name:  "my-app"
  --   container.googleapis.com/instance_id:  "7937682100289506354"
  --   container.googleapis.com/namespace_name:  "default"
  --   container.googleapis.com/pod_name:  "my-app-3144516956-msvv1"
  --   container.googleapis.com/stream:  "stdout"
  -- }
  --
  let resource = "a-resource"
  return resource

logMsg :: Text -> Text -> Text -> Text -> IO (Rs EntriesWrite)
logMsg clusterName namespace logName msg = do
  lgr <- newLogger Google.Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ loggingWriteScope)

  let
    entry = logEntry & leTextPayload ?~ msg
    entries = [entry]
    resourceLabels = monitoredResourceLabels $ HM.fromList
      [ ("cluster_name", clusterName)
      , ("namespace_id", namespace)
      ]
    resource = monitoredResource
      & mrType ?~ "container"
      & mrLabels ?~ resourceLabels
    labels = writeLogEntriesRequestLabels $ HM.fromList
      [ ("container.googleapis.com/cluster_name", clusterName)
      , ("container.googleapis.com/namespace_name", namespace)
      ]

  runResourceT . runGoogle env $
    send
      (entriesWrite
        (writeLogEntriesRequest
          & wlerEntries .~ entries
          & wlerLogName ?~ logName
          & wlerResource ?~ resource
          & wlerLabels ?~ labels
        )
      )

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [clusterName, namespace, logName, msg] ->
      handle (\e -> pprint ("exception" :: Text, e :: SomeException)) $ do
        putStrLn "Writing log message..."
        result <- logMsg (T.pack clusterName) (T.pack namespace) (T.pack logName) (T.pack msg)
        pprint ("result" :: Text, result)
    _ -> putStrLn "usage: google-log cluster-name namespace log-name message"
