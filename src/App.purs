module App 
    ( module App.Monad
    , appUI
    , Query (..)
    )
    where
  

import Prelude

import App.Monad (AppEnv, App, runAppM)
import App.Navigation (navigate)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Router (Route(..))
import Page (PageSlots, _homePage, homeComponent, _notfoundPage, notfoundComponent)

type State =
  { currentPage :: Route
  } 

data Query a 
    = Goto Route a
    | LocationChange Route a

initApp :: Route -> State 
initApp route  = { currentPage: route }

appUI :: H.Component HH.HTML Query Route Void App
appUI = 
    H.component
        { initialState: initApp
        , render
        , eval
        , receiver: const Nothing 
        , initializer: Nothing
        , finalizer: Nothing
        }
  where
    render :: State -> H.ComponentHTML Query PageSlots App
    render st =
        HH.div_ 
            [ viewPage st.currentPage ]

    viewPage :: Route -> H.ComponentHTML Query PageSlots App
    viewPage path = case path of
        Home -> HH.slot _homePage unit homeComponent unit absurd
        _    -> HH.slot _notfoundPage unit notfoundComponent unit absurd
    
    eval :: Query ~> H.HalogenM State Query PageSlots Void App
    eval (Goto route next) = do
      navigate route
      pure next
    eval (LocationChange route next) = do
      void $  H.modify ( _ { currentPage = route } )
      pure next

    