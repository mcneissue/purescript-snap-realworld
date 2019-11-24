module Test.Api.Types where

import Prelude

import Api.Types (Url)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (Either, either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throwException)
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

-- Like runTestM, but throws exceptions on Left
runTestM' :: Config -> TestM ~> Aff
runTestM' cfg tm = runTestM tm cfg
               >>= either (liftEffect <<< throwException) pure