module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as R
import Routing.PushState (makeInterface)
import App as App
import Simple.JSON (write)

main :: Effect Unit
main = HA.runHalogenAff $ do
  nav      <- liftEffect $ makeInterface
  location <- liftEffect $ nav.locationState

  body     <- HA.awaitBody
  let appUI           = H.hoist (App.runAppM { navInterface: nav, state: 1 }) App.appUI
      currentRoute = R.Home

  app <- runUI appUI currentRoute body

  void $ liftEffect $ nav.listen $ browserRouteChangeHandler (app.query)

  where                                                        
    browserRouteChangeHandler appQuery location = do
      log location.hash
      launchAff_ $ appQuery $ H.action $ (App.LocationChange $ R.parseRoute location.hash)
    
