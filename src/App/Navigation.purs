module App.Navigation where

import Router (Route)
import Halogen (HalogenM, lift)
import Prelude (class Monad, Unit, (<<<))

-- | DSL for navigating to a route.
class Monad m <= NavigateDSL m where
  navigate :: Route -> m Unit

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance navigationDSLHalogenM :: NavigateDSL m => NavigateDSL (HalogenM s f g p o m) where
    navigate = lift <<< navigate