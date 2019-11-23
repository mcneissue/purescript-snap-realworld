module Api.Util where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Api.Types (ApiError(..), Url)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (MultipleErrors)
import Model (Token)
import Prim.Row as Row
import Record as Record
import Simple.JSON as JSON

request :: forall m a
         . MonadAff m
        => AX.Request a
        -> m (Either ApiError (AX.Response a))
request = map (lmap AffjaxError) <<< liftAff <<< AX.request

authReq :: forall m r a
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => Either Method CustomMethod
        -> Maybe RequestBody
        -> Maybe Token
        -> ResponseFormat a
        -> String
        -> m (Either ApiError (AX.Response a))
authReq method mcontent mt rf url = do
  root <- asks (_.apiUrl >>> unwrap)
  let req = maybe AX.defaultRequest defaultAuthReq mt
  request $ req 
    { url = root <> url
    , responseFormat = rf
    , method = method
    , content = mcontent 
    }

defaultAuthReq :: Token -> AX.Request Unit
defaultAuthReq token = AX.defaultRequest 
  { headers = [ RequestHeader "Token" (unwrap token) ]
  }

parseAuthMethod :: forall m r a
                 . MonadAsk { apiUrl :: Url | r } m
                => MonadAff m
                => Either Method CustomMethod
                -> Maybe Token
                -> String
                -> Maybe RequestBody
                -> (String -> Either ApiError a)
                -> m (Either ApiError a)
parseAuthMethod method mt endpoint body parser =
  map (_ >>= parser <<< _.body) $ authReq method body mt ResponseFormat.string endpoint

parseAuthGet :: forall m r a
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => Maybe Token
             -> String
             -> (String -> Either ApiError a)
             -> m (Either ApiError a)
parseAuthGet t s p = parseAuthMethod (Left GET) t s Nothing p

parseGet :: forall m r a
          . MonadAsk { apiUrl :: Url | r } m
         => MonadAff m
         => String
         -> (String -> Either ApiError a)
         -> m (Either ApiError a)
parseGet = parseAuthGet Nothing

parseAuthPost :: forall m r a
               . MonadAsk { apiUrl :: Url | r } m
              => MonadAff m
              => Maybe Token
              -> String
              -> Maybe RequestBody
              -> (String -> Either ApiError a)
              -> m (Either ApiError a)
parseAuthPost = parseAuthMethod $ Left POST

parsePost :: forall m r a
           . MonadAsk { apiUrl :: Url | r } m
          => MonadAff m
          => String
          -> Maybe RequestBody
          -> (String -> Either ApiError a)
          -> m (Either ApiError a)
parsePost = parseAuthPost Nothing

parseAuthPut :: forall m r a
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => Maybe Token
             -> String
             -> Maybe RequestBody
             -> (String -> Either ApiError a)
             -> m (Either ApiError a)
parseAuthPut = parseAuthMethod $ Left PUT

parseAuthDelete :: forall m r a
                 . MonadAsk { apiUrl :: Url | r } m
                => MonadAff m
                => Maybe Token
                -> String
                -> (String -> Either ApiError a)
                -> m (Either ApiError a)
parseAuthDelete t s p = parseAuthMethod (Left DELETE) t s Nothing p

buildQuery :: Map String String -> String -> String
buildQuery params url | Map.isEmpty params = url
                      | otherwise = url <> "?" <> foldMapWithIndex go unfolded
  where
  unfolded :: Array (Tuple String String)
  unfolded = Map.toUnfoldable params

  go 0 (Tuple k v) = k <> "=" <> v
  go n (Tuple k v) = "&" <> k <> "=" <> v

readJson :: forall a. JSON.ReadForeign a => String -> Either ApiError a
readJson = lmap ParseError <<< JSON.readJSON

-- Helper for unwrapping responses like { foo :: Foo }
readJson' :: forall l r a. JSON.ReadForeign a => JSON.ReadForeign (Record r) => IsSymbol l => Row.Cons l a () r => SProxy l -> String -> Either ApiError a
readJson' _ s = bimap ParseError (Record.get (SProxy :: SProxy l)) $ (JSON.readJSON s :: Either MultipleErrors (Record r))

mkJsonBody :: forall a. JSON.WriteForeign a => a -> RequestBody
mkJsonBody = RequestBody.string <<< JSON.writeJSON