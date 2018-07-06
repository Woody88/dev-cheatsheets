module Page.Cheatsheet where


import Prelude
import Control.Monad (liftM1)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes
import App.Monad (App)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Halogen (ClassName(..), HalogenQ(..), lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Html.Parser.Halogen as PH
import Github.Types

type State = 
    { name :: String 
    }

type Input = State 

data Query a 
    = HandleInput Input a
    | Initializer a 

type Slot = H.Slot Query Void

component :: H.Component HH.HTML Query Input Void App
component = 
    H.component
        { initialState: (\i -> i)
        , render
        , eval
        , receiver: HE.input HandleInput
        , initializer: Just $ H.action Initializer
        , finalizer: Nothing
        }
  where
    render :: State -> H.ComponentHTML Query () App
    render st = do
        HH.div [ HP.class_ (ClassName "uk-text-center") ]
            [ HH.h3 [ HP.class_ (ClassName "uk-heading-primary") ] 
                [ HH.text st.name 
                , HH.span [ HP.class_ (ClassName "uk-text-muted") ] 
                    [ HH.text " Cheatsheet" ]  
                ]
            -- , HH.div_ ( map renderCheatsheet data_)
            ]

    -- renderCheatsheet :: CheatData -> H.ComponentHTML Query () App
    -- renderCheatsheet cheatdata = do
    --     HH.div_ 
    --         [ HH.h4 [ HP.class_ (ClassName "uk-heading-line") ] 
    --             [ HH.span_ [ HH.text cheatdata.title ]
    --             , PH.render $ cheatdata.html
    --             ]
    --         ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (Initializer next) = do
      oldState <- H.get
      liftEffect $ log "oldState"
      liftEffect $ logShow oldState
      H.liftAff $ getCheatsheets $ GitDir oldState.name
      pure next
    eval (HandleInput state next) = do
      oldState <- H.get
      when (oldState /= state) $ H.put state
      pure next

repo = "https://api.github.com/repos/Woody88/dev-cheatsheets/contents/"

getCheatsheets :: GitDir -> Aff Unit
getCheatsheets dir = do
   datasMaybe <- loadCheatsheetFiles dir
   let dataList = map loadFile $ fromMaybe [] datasMaybe
   liftEffect $ log "getCheatsheets"
   liftEffect $ logShow datasMaybe
   --liftEffect $ logShow dataList
   pure unit
   
loadFile :: GitData -> Aff (Maybe CheatData)
loadFile json = do
    res <- AX.affjax AXRes.json (AX.defaultRequest { url = json.url, method = Left GET })
    pure $ eitherToMaybe $ parseCheatData $ J.stringify res.response

loadCheatsheetFiles :: GitDir -> Aff (Maybe GitDatas)
loadCheatsheetFiles (GitDir dir) =  do
   res <-  AX.affjax AXRes.json (AX.defaultRequest { url = repo <> dir, method = Left GET })
   pure $ eitherToMaybe $ parseGitData $ J.stringify res.response

eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v 

-- This will be replaced by github queries
-- loadFiles :: Array FilePath -> String -> Effect (Array CheatData)
-- loadFiles files dir = sequence $ map loadFile files
--     where cheatdata fname h = pure { title: fname, html: h }
--           loadFile filename = readTextFile UTF8 ("./Cheatsheets/" <> dir <> filename) >>= cheatdata filename

-- loadCheatsheetFiles :: State -> Effect (Array String)
-- loadCheatsheetFiles state = readdir dirPath
--     where dirPath = "./Cheatsheets/" <> state.name

foreign import data Markdown :: Type
foreign import markdown :: Markdown
foreign import markdownRender :: String -> Markdown -> Effect String
