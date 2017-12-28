module Config exposing (configs)

import Config.Model as Config exposing (Model)
import Dict exposing (..)
import LocalConfig exposing (localConfigs)




configs : Dict String Model
configs =
    localConfigs
