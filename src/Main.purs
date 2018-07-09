module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement)
import Router as R
import Routing.PushState (makeInterface, PushStateInterface)
import App as App
import Component.HtmlHead as AppHeader

main :: Effect Unit
main = HA.runHalogenAff $ do
  HA.awaitLoad
  nav      <- liftEffect $ makeInterface
  location <- liftEffect $ nav.locationState
  body     <- HA.awaitBody
  
  let appUI        = H.hoist (App.runAppM { navInterface: nav, state: 1 }) App.appUI
      currentRoute = R.Home
  
  titleChangeHandler nav =<< (HA.selectElement $ QuerySelector "head")
  
  app <- runUI appUI currentRoute body

  void $ liftEffect $ nav.listen $ browserRouteChangeHandler (Just app.query) Nothing

  where 

    browserRouteChangeHandler Nothing (Just appHeadQuery) location = do
      log location.path
      launchAff_ $ appHeadQuery $ H.action $ (AppHeader.ChangeTitle $ R.parseRoute location.hash)
                                                
    browserRouteChangeHandler (Just appQuery) Nothing location = do
      log location.path
      launchAff_ $ appQuery $ H.action $ (App.LocationChange $ R.parseRoute location.hash)

    browserRouteChangeHandler _ _ _ = pure unit

    titleChangeHandler nav Nothing = pure unit
    titleChangeHandler nav (Just headTag) = do
       appHead <- runUI AppHeader.component unit headTag
       void $ liftEffect $ nav.listen $ browserRouteChangeHandler Nothing (Just appHead.query) 
  
      