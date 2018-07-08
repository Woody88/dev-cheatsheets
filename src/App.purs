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
import Effect.Console (log)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Page
import Router (Route(..))
import Github.Types (initCheatData)

type State =
  { currentPage  :: Route
  , previousPage :: Route
  } 

data Query a 
    = Goto Route a
    | LocationChange Route a

initApp :: Route -> State 
initApp route  = { currentPage: route, previousPage: route }

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
            [ navBar st.previousPage
            , viewPage st.currentPage 
            ]
            
    navBar :: Route -> H.ComponentHTML Query Slots App
    navBar prevRoute = HH.slot _navbar unit navbarComponent prevRoute absurd

    viewPage :: Route -> H.ComponentHTML Query Slots App
    viewPage path = case path of
        Home           -> HH.slot _homePage unit homeComponent unit absurd
        (Cheatsheet p) -> HH.slot _cheatsheetPage unit cheatsheetComponent {name: show p, cheatdata: [initCheatData]} absurd
        otherwise      -> HH.slot _notfoundPage unit notfoundComponent unit absurd
        
    eval :: Query ~> H.HalogenM State Query Slots Void App
    eval (Goto route next) = do
      void $  H.modify (\s ->  s { previousPage = s.currentPage} )
      navigate route
      pure next
    eval (LocationChange route next) = do
      void $  H.modify (\s ->  s { currentPage = route, previousPage = s.currentPage} )
      pure next

