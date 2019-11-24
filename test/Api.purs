module Test.Api where

import Prelude

import Api as Api
import Api.Types (ApiError, Url(..))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT, throwError, class MonadError)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks, class MonadAsk, ask)
import Data.Array (length)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Posix.Signal (Signal(..))
import Data.Posix.Signal as Signal
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, error)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error(..), throwException)
import Foreign (MultipleErrors)
import Foreign.Object as Object
import Model (Article, Token(..), UpdateUser, User)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON
import Snap.SYTC.Component (identity)
import Test.Spec (SpecT(..), around_, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Equality (class TypeEquals, from)

type EnvVar = { key :: String, value :: String }

type Config =
  { startServer :: String
  , workingDir :: String
  , apiUrl :: Url
  , cleanup :: String
  , envVars :: Array EnvVar
  }
  
newtype TestM a = TestM (ReaderT Config (ExceptT Error Aff) a)

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance moandAffTestM :: MonadAff TestM
derive newtype instance monadErrorTestM :: MonadError Error TestM
derive newtype instance monadThrowTestM :: MonadThrow Error TestM

instance monadAskTestM :: TypeEquals e Config => MonadAsk e TestM where
  ask = TestM $ asks from

runTestM :: forall a. TestM a -> Config -> Aff (Either Error a)
runTestM (TestM a) = runExceptT <<< runReaderT a

parseConfig :: String -> Either MultipleErrors Config
parseConfig = JSON.readJSON

readConfig :: Aff Config
readConfig = unsafePartial (fromRight <<< parseConfig) <$> FS.readTextFile UTF8 "./test/config.json"

startServer :: forall m. MonadAff m => Config -> m ChildProcess
startServer c = do
  cp  <- spawnCmd c.workingDir c.envVars c.startServer
  er <- liftAff $ runTestM (awaitServer 20) c
  case er of
    Right _ -> pure cp
    Left e  -> liftEffect $ throwException e

awaitServer :: Int -> TestM Unit
awaitServer retry = do
  -- liftEffect $ log "starting server"
  liftAff $ delay (Milliseconds 100.0)
  void testGetArticles `catchError` 
    \e -> if retry >= 0 
            then awaitServer (retry - 1) 
            else throwError e

cleanupServer :: forall m. MonadAff m => Config -> ChildProcess -> m Unit
cleanupServer c cp = do
  liftEffect $ CP.kill Signal.SIGKILL cp
  void $ runCmd_ c.envVars c.cleanup

withServer :: TestM Unit -> TestM Unit
withServer action = do
  ctx <- ask
  er <- liftAff $ bracket
    (startServer ctx)
    (cleanupServer ctx)
    (const $ runTestM action ctx)
  case er of
    Right _ -> pure unit
    Left e -> liftEffect $ throwException e

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
  pure as

testuser :: { username :: String, email :: String, password :: String }
testuser =
  { username: "testuser"
  , email: "test@test.com"
  , password: "testing123" 
  }

register :: { username :: String, email :: String, password :: String } -> TestM User
register = liftE <<< Api.postRegister

liftE :: forall a. TestM (Either ApiError a) -> TestM a
liftE a = a >>= either (show >>> error >>> throwError) pure

updatedUser :: UpdateUser
updatedUser =
  { email: "newemail@test.com"
  , username: "newusername"
  , bio: "newbio"
  , image: "newimage"
  , password: Just $ "newpassword123"
  }

apiSpec :: forall m. Monad m => SpecT TestM Unit m Unit
apiSpec = around_ withServer do
  describe "API Client" do
    describe "Auth" do
      it "can register a user" do
        user <- register testuser
        user.username `shouldEqual` testuser.username
      it "can login" do
        _ <- register testuser
        _ <- login { email: testuser.email, password: testuser.password }
        pure unit
    describe "User" do
      before (_.token <$> register testuser) do
        it "gets the current user" (void <<< liftE <<< Api.getCurrentUser)
        it "can update the current user" $ \t -> do
          _ <- liftE $ Api.putUser t updatedUser
          _ <- login { email: updatedUser.email, password: unsafePartial (fromJust updatedUser.password) }
          pure unit
