module Test.Api where

import Prelude

import Api as Api
import Api.Types (ApiError, Url)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Posix.Signal as Signal
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, error)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throwException)
import Foreign (MultipleErrors)
import Foreign.Object as Object
import Model (Article, CreateArticle, Token, UpdateArticle, UpdateUser, User)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON
import Test.Spec (SpecT, around_, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Equality (class TypeEquals, from)

type EnvVar = { key :: String, value :: String }

type StartServer =
  { executable :: String
  , arguments :: Array String
  }

type Config =
  { startServer :: StartServer
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
  cp  <- spawnCmd c.workingDir c.envVars c.startServer.executable c.startServer.arguments
  er <- liftAff $ runTestM (awaitServer 20) c
  case er of
    Right _ -> pure cp
    Left e  -> liftEffect $ throwException e

awaitServer :: Int -> TestM Unit
awaitServer retry = do
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

spawnCmd :: forall m. MonadEffect m => String -> Array EnvVar -> String -> Array String -> m ChildProcess
spawnCmd workDir vs cmd args = liftEffect $ CP.spawn cmd args opts
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

type RegisterUser = { username :: String, email :: String, password :: String }

testuser :: RegisterUser
testuser =
  { username: "testuser"
  , email: "test@test.com"
  , password: "testing123" 
  }

testuser2 :: RegisterUser
testuser2 =
  { username: "testuser2"
  , email: "test2@test.com"
  , password: "testing123"
  }

register :: RegisterUser -> TestM User
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

testArticle :: CreateArticle
testArticle =
  { title: "test"
  , description: "test"
  , body: "test"
  , tagList: ["test", "test2"]
  }

testArticleUpdate :: UpdateArticle
testArticleUpdate = testArticle { title = "updated test" }

createArticle :: Token -> TestM Article
createArticle t = liftE $ Api.postArticle t testArticle

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
        it "can update the current user" \t -> do
          _ <- liftE $ Api.putUser t updatedUser
          _ <- login { email: updatedUser.email, password: unsafePartial (fromJust updatedUser.password) }
          pure unit
    describe "Profile" do
      before (register testuser2 *> (_.token <$> register testuser)) do
        it "can follow a profile" \t -> void $ liftE $ Api.postFollow t "testuser2"
        it "can get a profile" $ const $ void $ liftE $ Api.getProfile "testuser2"
        it "can unfollow a profile" \t -> void $ liftE $ Api.deleteFollow t "testuser2"
    describe "Articles" do
      before (_.token <$> register testuser) do
        it "can create an article" (void <<< createArticle)
        it "can delete an article" \t -> do
          a <- createArticle t
          void $ liftE $ Api.deleteArticle t a.slug
        it "can update an article" \t -> do
          a <- createArticle t
          void $ liftE $ Api.putArticle t a.slug testArticleUpdate
        it "can get an article list" $ const $ void $ liftE $ Api.getArticles Api.defaultGetArticlesParams
        it "can get an article feed" \t -> void $ liftE $ Api.getFeed t Api.defaultGetFeedParams
        it "can favorite/unfavorite an article" \t -> do
          t2 <- _.token <$> register testuser2
          a <- createArticle t2
          void $ liftE $ Api.postFavoriteArticle t a.slug
          a' <- liftE $ Api.deleteFavoriteArticle t a.slug
          a'.favorited `shouldEqual` false
    describe "Comments" do
      before commentsSetup do
        it "can comment on an article" \ctx -> void $ liftE $ Api.postComment ctx.token ctx.slug { body: "foo" }
        it "can get comments" \ctx -> void $ liftE $ Api.getComments ctx.slug
        it "can delete a comment" \ctx -> do
          c <- liftE $ Api.postComment ctx.token ctx.slug { body: "foo" }
          void $ liftE $ Api.deleteComment ctx.token ctx.slug c.id
    describe "Tags" do
      it "can fetch tags" $ void $ liftE $ Api.getTags

commentsSetup :: TestM { token :: Token, slug :: String }
commentsSetup = do
  t <- _.token <$> register testuser2
  slug <- _.slug <$> createArticle t
  token <- _.token <$> register testuser
  pure { slug, token }