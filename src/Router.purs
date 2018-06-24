module Router where
  
import Prelude

import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (stripPrefix, Pattern(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Routing (match)
import Routing.Match (Match, lit, root, end)

data Query a 
    = Goto Route a
    | LocationChange String a

data Message 
    = RouteChanged String

data Route
    = Home
    | Page
    | NotFound
    

instance showRoutes :: Show Route where
  show (Home)     = "Home"
  show (Page)     = "Page"
  show (NotFound) = "NotFound"

type State =
  { currentPage :: String
  }

parseRoute :: String -> Route
parseRoute = parseRouteEither <<< match routing <<< stripPrefixBackslash
    where parseRouteEither = either (const NotFound) identity
          stripPrefixBackslash "/" = "/"
          stripPrefixBackslash str = fromMaybe "404" $ stripPrefix (Pattern "/") str

routing :: Match Route
routing = oneOf
    [ home
    , page
    , pure NotFound
    ]
    where 
          home = Home <$ root <* end
          page = Page <$ lit "page" <* end

router :: H.Component HH.HTML Query String Message Aff
router = 
    H.component
        { initialState: (\path -> { currentPage: show $ parseRoute path })
        , render
        , eval
        , receiver: const Nothing 
        }
  where
    render :: State -> H.ComponentHTML Query
    render st =
        HH.div_ 
            [ HH.h1_ [ HH.text (st.currentPage) ] 
            , viewPage st.currentPage
            ]

    viewPage :: String -> H.ComponentHTML Query
    viewPage path = case path of
        "Home" -> HH.div_ [ HH.button [ HE.onClick (HE.input_ $ Goto Page) ] [ HH.text "Not Found" ] ]
        "Page" -> HH.div_ [ HH.p_ [] ]
        _      -> HH.p_ [] 
    
    eval :: Query ~> H.ComponentDSL State Query Message Aff
    eval (Goto NotFound next) = do
      void $  H.modify (\s -> s { currentPage = "NotFound" })
      pure next
    eval (Goto Page next) = do
      void $  H.modify (\s -> s { currentPage = "Page" })
      H.raise $ RouteChanged "/page"
      pure next
    eval (Goto Home next) = do
      void $  H.modify (\s -> s { currentPage = "Home" })
      H.raise $ RouteChanged "/"
      pure next
    eval (LocationChange str next) = do
      void $  H.modify (\s -> s { currentPage = show $ parseRoute str })
      pure next

      