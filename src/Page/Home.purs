module Page.Home where
  
import Prelude
import Router

import App.Monad (App)
import App.Navigation (navigate)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..), AttrName(..))
import Halogen as H
import Halogen.HTML (i)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Int

data Query a 
    = Goto Route a

type Slot = H.Slot Query Void

component :: H.Component HH.HTML Query Unit Void App
component = 
    H.component
        { initialState: const 0
        , render
        , eval
        , receiver: const Nothing 
        , initializer: Nothing
        , finalizer: Nothing
        }
  where
    render :: State -> H.ComponentHTML Query () App
    render st =
        HH.div [ HP.class_ (ClassName "uk-section") ]
            [ HH.div [ HP.class_ (ClassName "uk-container uk-text-center") ] 
                [ HH.h1 [ HP.class_ (ClassName "uk-heading-primary") ] 
                    [ HH.text "Woody's cheatsheets" ]
                , HH.p_ [ HH.text "Hello, Welcome to my cheatsheets site. This site is greatly inspired from devhints.io" ]
                , searchForm st
                ]
            , recentlyUpdated
            ] 
            
    searchForm :: State -> H.ComponentHTML Query () App
    searchForm state = 
        HH.div [ HP.class_ (ClassName "cheatsheet-search uk-margin-small uk-width-1-2 uk-align-center uk-card uk-card-default uk-search-medium") ]
            [ HH.form [ HP.class_ (ClassName "uk-search uk-padding-small uk-width-1-1") ] 
                [ HH.a [ HP.class_ (ClassName "uk-search-icon-flip"), HP.attr (AttrName "uk-search-icon") ""] []
                , HH.input [ HP.class_ (ClassName "uk-search-input uk-form-width-large"), HP.type_ HP.InputSearch, HP.placeholder "/ Search..." ] 
                ]
            ]
    recentlyUpdated = 
        HH.div [ HP.class_ (ClassName "uk-margin") ] 
            [ HH.text "Recently updated" 
            , HH.hr []
            , HH.div [ HP.class_ (ClassName "uk-column-1-2@s uk-column-1-3@m uk-column-1-4@l") ]
                (map recentlyUpdatedItems items)
            ]

    recentlyUpdatedItems item = 
        HH.a [ HP.class_ (ClassName "uk-link-heading"), HE.onClick (HE.input_ (Goto $ Cheatsheet item)) ]
            [ HH.div [ HP.class_ (ClassName "uk-card uk-card-small uk-card-default uk-padding-small recent-items uk-text-middle") ]
                [ HH.h3_ [ HH.text $ show item ] ]
            ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (Goto route next) = do
      navigate route
      pure next

items :: Array Page
items = [ Purescript, Haskell, Salesforce, Bash ]

