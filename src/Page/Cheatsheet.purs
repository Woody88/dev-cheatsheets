module Page.Cheatsheet where

import Github.Types
import Prelude

import App.Monad (App)
import Data.String
import Data.String.Pattern (Pattern)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Html.Parser.Halogen as PH
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes
import Text.Base64 (decode64)

type State = 
    { name :: String 
    , cheatdata :: Array CheatData
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
        HH.div []
            [ HH.div [ HP.class_ (ClassName "uk-margin-xlarge-bottom uk-text-center") ]
                [ HH.h3 [ HP.class_ (ClassName "uk-heading-primary cheatsheet-header") ] 
                    [ HH.text st.name 
                    , HH.span [ HP.class_ (ClassName "cheatsheet-title") ] 
                        [ HH.text " Cheatsheet" ]  
                    ]
                ]
            , HH.div [HP.class_ (ClassName "uk-child-width-1-3@m"), HP.attr (AttrName "uk-grid") ""] 
                (renderCheats st.cheatdata)
            ]
            where renderCheats [] = [ HH.h2 [ HP.class_ (ClassName "uk-heading-bullet") ] [ HH.text "Under Construction!" ] ]
                  renderCheats cheats = map renderCheatsheet cheats

    renderCheatsheet :: CheatData -> H.ComponentHTML Query () App
    renderCheatsheet cheatdata = do
        HH.div [ HP.class_ (ClassName "cheatsheet-code-snippet")]
            [ HH.h4 [ HP.class_ (ClassName "uk-text-capitalize uk-heading-line uk-text-left cheatsheet-topic") ] 
                [ HH.span_ [ HH.text cheatdata.name ]
                ]
            , PH.render cheatdata.content
            ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (Initializer next) = do
      oldState <- H.get
      liftEffect $ log "oldState"
      liftEffect $ logShow oldState
      datas <- H.liftAff $ getCheatsheets $ GitDir oldState.name
      H.modify_ (_ { cheatdata = datas })
      pure next
    eval (HandleInput state next) = do
      oldState <- H.get
      when (oldState /= state) $ H.put state
      pure next

formatCheatsheetTitle :: String -> String
formatCheatsheetTitle = replaceUnderscore <<< replaceMdExt
    where 
        replaceMdExt = replace (Pattern ".md") (Replacement "")
        replaceUnderscore = replace (Pattern "_") (Replacement " ")


getCheatsheets :: GitDir -> Aff (Array CheatData)
getCheatsheets dir = do
   datasMaybe <- loadCheatsheetFiles dir
   
   let m = fromMaybe [] datasMaybe
   dataList <- traverse loadFile    m
   let sequencedDataList = sequence dataList
   parsedData <- liftEffect $ traverse parseContent (fromMaybe [] sequencedDataList)
   liftEffect $ log "getCheatsheets"
   liftEffect $ logShow datasMaybe
   liftEffect $ logShow sequencedDataList
   pure parsedData
   
parseContent :: CheatData -> Effect CheatData
parseContent data_ = do
  md <- markdownRender (decode64 data_.content) markdown
  pure $ data_ { content = md, name = formatCheatsheetTitle data_.name }

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