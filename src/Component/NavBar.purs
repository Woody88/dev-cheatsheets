module Component.NavBar where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Router (Route(..))
import App.Monad (App)
import App.Navigation (navigate)

type State   = Route
type Input   = Route
type Message = Void 

type Slot = H.Slot Query Void

data Query a 
    = HandleInput Route a
    | Goto Route a

component :: H.Component HH.HTML Query Input Message App
component = 
    H.component
        { initialState: const Home
        , render
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }
    where 
        render :: State -> H.ComponentHTML Query () App
        render state = 
            HH.nav [ HP.class_ (ClassName "uk-text-bold uk-navbar-container uk-navbar-transparent uk-margin"), HP.attr (AttrName "uk-navbar") "" ] 
                [ navbarLeft state
                , navbarCenter
                ]

        eval :: Query ~> H.HalogenM State Query () Message App
        eval (Goto route next) = do
            navigate route
            pure next
        eval (HandleInput route next) = do
            oldState <- H.get
            when (oldState /= route) $ H.put route
            pure next

        navbarLeft prevRoute = 
            HH.div [ HP.class_ (ClassName "uk-navbar-left") ]
                [ HH.ul [ HP.class_ (ClassName "uk-navbar-nav") ]
                    [ HH.li_
                        [ HH.a [ HP.class_ (ClassName "uk-icon-link back-button"), HP.attr (AttrName "uk-icon") "icon: arrow-left; ratio: 2", HE.onClick (HE.input_ $ Goto prevRoute) ]
                            []
                        ]
                    ]
                ]

        navbarCenter = 
            HH.div [ HP.class_ (ClassName "uk-navbar-center") ]
                [ HH.ul [ HP.class_ (ClassName "uk-navbar-nav") ]
                    [ HH.li [ HP.class_ (ClassName "uk-active") ]
                        [ HH.a [ HP.class_ (ClassName "uk-navbar-item uk-logo"), HP.href "#" ]
                            [ HH.text "Dev-Cheatsheet" ]
                        ]
                    ]
                ]
        
        navbarRight = 
            HH.div [ HP.class_ (ClassName "uk-navbar-right") ] []
