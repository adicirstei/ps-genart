module RAF where


import Prelude

import Effect (Effect)


foreign import requestAnimationFrame :: Effect Unit -> Effect Unit

