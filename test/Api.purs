module Test.Api where

import Prelude

import Api as Api
import Api.Types (ApiError, Url(..))
import Control.Monad.Except (ExceptT(..), except, throwError, runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.Posix.Signal as Signal
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Foreign (MultipleErrors)
import Foreign.Object as Object
import Model (User, Article)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON
import Snap.SYTC.Component (identity)

type EnvVar = { key :: String, value :: String }

type Config =
  { startServer :: String
  , workingDir :: String
  , apiUrl :: Url
  , cleanup :: String
  , envVars :: Array EnvVar
  }
  
type TestM a = ReaderT Config (ExceptT ApiError Aff) a

runTestM :: forall a. TestM a -> Config -> Aff (Either ApiError a)
runTestM a = runExceptT <<< runReaderT a

parseConfig :: String -> Either MultipleErrors Config
parseConfig = JSON.readJSON

readConfig :: Aff Config
readConfig = unsafePartial (fromRight <<< parseConfig) <$> FS.readTextFile UTF8 "./test/config.json"

startServer :: TestM ChildProcess
startServer = do
  cmd <- asks _.startServer
  vs  <- asks _.envVars
  wd  <- asks _.workingDir
  spawnCmd wd vs cmd

cleanupServer :: ChildProcess -> TestM Unit
cleanupServer cp = do
  cmd <- asks _.cleanup
  vs  <- asks _.envVars
  liftEffect $ CP.kill Signal.SIGKILL cp
  void $ runCmd_ vs cmd

runCmd_ :: forall m. MonadEffect m => Array EnvVar -> String -> m Unit
runCmd_ vs cmd = liftEffect $ void $ CP.execSync cmd opts
  where
  opts = CP.defaultExecSyncOptions
    { env = Just $ foldMap (\({ key, value }) -> Object.singleton key value) vs
    }

spawnCmd :: forall m. MonadEffect m => String -> Array EnvVar -> String -> m ChildProcess
spawnCmd workDir vs cmd = liftEffect $ CP.spawn cmd [] opts
  where
  opts = CP.defaultSpawnOptions
    { env = Just $ foldMap (\({ key, value }) -> Object.singleton key value) vs
    , cwd = Just workDir
    }

login :: { email :: String, password :: String } -> TestM User
login creds = liftE $ Api.postLogin creds

testGetArticles :: TestM (Array Article)
testGetArticles = do
  as <- liftE $ Api.getArticles Api.defaultGetArticlesParams
  liftEffect $ logShow as
  pure as

register :: TestM User
register = liftE $ Api.postRegister 
  { username: "testuser"
  , email: "test@test.com"
  , password: "testing123" 
  }

liftE :: forall a. TestM (Either ApiError a) -> TestM a
liftE a = a >>= either throwError pure