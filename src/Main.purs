module Main 
  
where
  
import Data.Record.Pretty
import Prelude

import Control.Monad.Eff.Console (log)
import Prettier.Printer (pretty)
import Ansi.Codes
import Ansi.Output


alice = {name: "alice", age: 45}

author :: { name :: String, interests :: Array String }
author =
    { name: "Phil"
    , interests:
      [ "Functional Programming"
      , "JavaScript"
      ]
    }

nested =
  { name: "bob"
  , age: 23
  , traits:
    { lucky: { colors: ["red", "green"]
             , number: 45
             }
    , unlucky: { signs: ["aquarius"]
               , days: ["monday", "sunday"]
               }
    }
  , hobbies:
    [ { hobby: "fishing"
      , daysActive: 4
      }
    , { hobby: "running"
      , daysActive: 198
      }
    ]
  }

bigRecord =
  { author: author
  , friend: alice
  , metaData: nested
  }
  
main = do
  log $ pretty 5 $ ppRecord alice 
  log $ pretty 5 $ ppRecord author
  log $ pretty 10 $ ppRecord nested
  log $ pretty 20 $ ppRecord bigRecord
  log $ withGraphics bold "hello"
  log $ withGraphics (bold <> foreground Red) "something"
