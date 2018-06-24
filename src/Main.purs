module Main where

import Prelude

import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as R
import Routing.PushState (makeInterface)
import Simple.JSON (write)


main :: Effect Unit
main = HA.runHalogenAff $ do
  nav  <- liftEffect $ makeInterface
  
  body <- HA.awaitBody
  location <- liftEffect $ nav.locationState
  void $ liftEffect $ log location.path        --- Debug purpose
  
  app <- runUI R.router location.path body
  
  _ <- liftEffect $ nav.listen $ browserChangeHandler (app.query)

  app.subscribe $ CR.consumer \(R.RouteChanged path) -> do
    liftEffect $ nav.pushState (write {}) path
    pure Nothing
  
  where                                                        
    browserChangeHandler qry location = do
      log location.path
      launchAff_ $ qry $ H.action $ (R.LocationChange location.path)
    
      