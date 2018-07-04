module App 
    ( module App.Monad
    , appUI
    , Query (..)
    )
    where
  

import Prelude

import App.Monad (AppEnv, App, runAppM)
import App.Navigation (navigate)
import App.Slot (Slots, _homePage, _navbar, _notfoundPage, homeComponent, navbarComponent, notfoundComponent)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Router (Route(..))

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
    render :: State -> H.ComponentHTML Query Slots App
    render st =
        HH.div [ HP.class_ (ClassName "uk-container") ]
            [ navBar
            , viewPage st.currentPage 
            ]
            
    navBar :: H.ComponentHTML Query Slots App
    navBar = HH.slot _navbar unit navbarComponent unit absurd

    viewPage :: Route -> H.ComponentHTML Query Slots App
    viewPage path = case path of
        Home -> HH.slot _homePage unit homeComponent unit absurd
        _    -> HH.slot _notfoundPage unit notfoundComponent unit absurd
    
    eval :: Query ~> H.HalogenM State Query Slots Void App
    eval (Goto route next) = do
      navigate route
      pure next
    eval (LocationChange route next) = do
      void $  H.modify ( _ { currentPage = route } )
      pure next
