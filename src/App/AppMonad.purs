module App.AppMonad where

import Prelude

import App.Navigation (class NavigateDSL)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Data.String (toLower)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)


type AppEnv =
  { navInterface :: PushStateInterface 
  , state :: Int 
  }

newtype App a = AppM (ReaderT AppEnv Aff a)

-- Natural transformation
runAppM :: AppEnv -> App ~> Aff 
runAppM env (AppM m) = flip runReaderT env $ m

derive newtype instance functorApp :: Functor App
derive newtype instance applyApp :: Apply App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance bindApp :: Bind App
derive newtype instance monadApp :: Monad App
derive newtype instance appMonadEffect :: MonadEffect App

instance navigateDSLAppM :: NavigateDSL App where
  navigate route = AppM do
    env <- ask
    log ("state : " <> show env.state)
    liftEffect $ env.navInterface.pushState (write {}) (toLower $ show route)

    