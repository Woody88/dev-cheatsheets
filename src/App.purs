module App 
    ( module App.Monad
    , appUI
    , Query (..)
    )
    where
  

import Prelude

import App.Monad (AppEnv, App, runAppM)
import App.Navigation (navigate)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Page.Cheatsheet as PC
import Page.Home as PH
import Router (Route(..))

type State =
  { currentPage :: Route
  } 

data Query a 
    = Goto Route a
    | LocationChange Route a

type ChildQuery = Coproduct2 PH.Query PC.Query
type ChildSlot = Either2 Unit Unit 

initApp :: Route -> State 
initApp route  = { currentPage: route }

appUI :: H.Component HH.HTML Query Route Unit App
appUI = 
    H.parentComponent
        { initialState: initApp
        , render
        , eval
        , receiver: const Nothing 
        }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot App
    render st =
        HH.div_ 
            [ viewPage st.currentPage ]

    viewPage :: Route -> H.ParentHTML Query ChildQuery ChildSlot App
    viewPage path = case path of
        Home ->  HH.slot' CP.cp1 unit PH.component unit absurd
        Page -> HH.slot' CP.cp2 unit PC.component unit absurd
        _    -> HH.p_ [ HH.text "Bad Route" ] 
    
    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Unit App
    eval (Goto route next) = do
      navigate route
      pure next
    eval (LocationChange route next) = do
      void $  H.modify ( _ { currentPage = route } )
      pure next

      