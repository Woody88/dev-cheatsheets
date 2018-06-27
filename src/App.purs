module App 
    ( module App.AppMonad
    , appUI
    , Query (..)
    )
    where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Router (Route(..))
import App.AppMonad (AppEnv, App, runAppM)
import App.Navigation (navigate)


type State =
  { currentPage :: Route
  } 

data Query a 
    = Goto Route a
    | LocationChange Route a

initApp :: Route -> State 
initApp route  = { currentPage: route }

appUI :: H.Component HH.HTML Query Route Unit App
appUI = 
    H.component
        { initialState: initApp
        , render
        , eval
        , receiver: const Nothing 
        }
  where
    render :: State -> H.ComponentHTML Query
    render st =
        HH.div_ 
            [ HH.h1_ [ HH.text (show $ st.currentPage) ] 
            , viewPage st.currentPage
            ]

    viewPage :: Route -> H.ComponentHTML Query
    viewPage path = case path of
        Home -> HH.div_ [ HH.button [ HE.onClick (HE.input_ $ Goto Page) ] [ HH.text "Not Found" ] ]
        Page -> HH.div_ [ HH.p_ [] ]
        _      -> HH.p_ [] 
    
    eval :: Query ~> H.ComponentDSL State Query Unit App
    eval (Goto route next) = do
      navigate route
      pure next
    eval (LocationChange route next) = do
      void $  H.modify ( _ { currentPage = route } )
      pure next

      