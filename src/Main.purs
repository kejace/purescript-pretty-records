module Main 
  
where
  
import Data.Record.Pretty
import Prelude

import Control.Monad.Eff.Console (log)
import Prettier.Printer (pretty)

myRecord = {name: "alice", age: 45}

author :: { name :: String, interests :: Array String }
author =
    { name: "Phil"
    , interests: ["Functional Programming", "JavaScript"]
    }

main = do
  log $ pretty 5 $ ppRecord myRecord
  log $ pretty 5 $ ppRecord author -- 
