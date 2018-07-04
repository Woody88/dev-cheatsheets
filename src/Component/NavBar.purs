module Component.NavBar where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP

type State   = Int
type Input   = Unit
type Message = Void 

type Slot = H.Slot Query Void

data Query a 
    = NoOp Unit a

component :: forall m. H.Component HH.HTML Query Unit Void m
component = 
    H.component
        { initialState: const 1
        , render
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }
    where 
        render :: State -> H.ComponentHTML Query () m
        render state = 
            HH.nav [ HP.class_ (ClassName "uk-navbar-container uk-navbar-transparent uk-margin"), HP.attr (AttrName "uk-navbar") "" ] 
                [ navbarLeft 
                , navbarCenter
                ]

        eval :: Query ~> H.HalogenM State Query () Void m
        eval (NoOp _ next) = 
            pure next

        navbarLeft = 
            HH.div [ HP.class_ (ClassName "uk-navbar-left") ]
                [ HH.ul [ HP.class_ (ClassName "uk-navbar-nav") ]
                    [ HH.li_
                        [ HH.a [ HP.class_ (ClassName "uk-icon-link"), HP.attr (AttrName "uk-icon") "icon: arrow-left; ratio: 2" ]
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
