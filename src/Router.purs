module Router where
  
import Prelude

import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Maybe (fromMaybe)
import Data.String (stripPrefix, Pattern(..))
import Routing (match)
import Routing.Match (Match, lit, root, end)

data Route
    = Home
    | Page
    | NotFound
    
instance showRoutes :: Show Route where
  show (Home)     = "home"
  show (Page)     = "page"
  show (NotFound) = "404"

parseRoute :: String -> Route
parseRoute = parseRouteEither <<< match routing <<< stripPrefixBackslash
    where parseRouteEither = either (const NotFound) identity
          stripPrefixBackslash "/" = "/"
          stripPrefixBackslash path = fromMaybe (show NotFound) $ stripPrefix (Pattern "/") path

routing :: Match Route
routing = oneOf
    [ home
    , page
    , pure NotFound
    ]
    where 
          home = Home <$ root <* end
          page = Page <$ lit "page" <* end


      