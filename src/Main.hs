{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Exception
import Control.Monad
import Data.Text (Text)
import Network.Google as Google
import Network.Google.Auth
import Network.Google.Logging
import System.IO (stdout)

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified System.Environment as Env

import qualified Network.Google.Compute.Metadata as Metadata

pprint :: Show a => a -> IO ()
pprint =
  putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

getEnv :: IO (Text, Text, Text, Text, Text)
getEnv = do
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
  manager <- newManager tlsManagerSettings
  projectId <- Metadata.getProjectId manager
  description <- Metadata.getDescription manager
  hostname <- Metadata.getHostname manager
  instanceId <- Metadata.getInstanceId manager
  zone <- Metadata.getZone manager
  return (projectId, description, hostname, instanceId, zone)

logMsg :: Bool -> Text -> Text -> Text -> Text -> IO (Rs EntriesWrite)
logMsg useEnv clusterName namespace logName msg = do
  when useEnv $ do
    (projectId, description, hostname, instanceId, zone) <- getEnv
    print ("projectId" :: Text, projectId)
    print ("description" :: Text, description)
    print ("hostname" :: Text, hostname)
    print ("instanceId" :: Text, instanceId)
    print ("zone" :: Text, zone)

  lgr <- newLogger Google.Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ loggingWriteScope)

  -- Try to pull out credentials to see what they've been set to.
  let store = view envStore env
  auth <- retrieveAuthFromStore store
  let credentials = _credentials auth
  case credentials of
    FromMetadata serviceId -> print ("FromMetadata" :: Text, serviceId)
    FromClient _a _b -> print ("FromClient" :: Text)
    FromAccount serviceAccount -> print ("FromAccount" :: Text, serviceAccount)
    FromUser user -> print ("FromUser" :: Text, user)

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
    [env, clusterName, namespace, logName, msg] ->
      handle (\e -> pprint ("exception" :: Text, e :: SomeException)) $ do
        putStrLn "Writing log message..."
        result <- logMsg (env == "yes") (T.pack clusterName) (T.pack namespace) (T.pack logName) (T.pack msg)
        pprint ("result" :: Text, result)
    _ -> putStrLn "usage: google-log env cluster-name namespace log-name message"
