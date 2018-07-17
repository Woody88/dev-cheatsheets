```haskell

{-- JS file
exports.unsafeSetProp = function(prop) {
  return function(obj) {
    return function(value) {
      return function() {
        obj[prop] = value;
      };
    };
  };
}; 
--}

import Web.HTML as DOM
import FRP.Event (Event)
import FRP.Event (create, subscribe) as Event
import Effect (Event)
import Foreign (Foreign)

main :: Effect Unit
main = do
    _ <- createWindowEvent "newobj" decodeObj
    pure unit
    where decodeObj _ = "Hello world!"

createWindowEvent :: forall a. String -> (Foreign -> a) -> Effect (Event a)
createWindowEventChannel name f = do
  { event, push } <- Event.create
  window <- DOM.window
  unsafeSetProp name window (unsafePerformEffect <<< push <<< f)
  pure event
```

```bash
window.newobj;
Hello World!
```