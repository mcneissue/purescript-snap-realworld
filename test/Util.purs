module Test.Util where

import Prelude (Unit, void, ($))

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object as Object
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP

import Test.Api.Types (EnvVar)

runCmd_ :: forall m. MonadEffect m => Array EnvVar -> String -> m Unit
runCmd_ vs cmd = liftEffect $ void $ CP.execSync cmd opts
  where
  opts = CP.defaultExecSyncOptions
    { env = Just $ foldMap (\({ key, value }) -> Object.singleton key value) vs
    }

spawnCmd :: forall m. MonadEffect m => String -> Array EnvVar -> String -> Array String -> m ChildProcess
spawnCmd workDir vs cmd args = liftEffect $ CP.spawn cmd args opts
  where
  opts = CP.defaultSpawnOptions
    { env = Just $ foldMap (\({ key, value }) -> Object.singleton key value) vs
    , cwd = Just workDir
    }