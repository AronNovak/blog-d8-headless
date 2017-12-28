module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        , Sidebar(..)
        )

import App.PageType exposing (Page(..))
import Config.Model
import Date exposing (Date)
import Error.Model exposing (Error)
import ItemManager.Model exposing (Model, emptyModel)
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (Time)
import Translate exposing (Language(English))


type Msg
    = HandleOfflineEvent (Result String Bool)
    | MsgItemManager ItemManager.Model.Msg
    | NoOp
    | SetActivePage Page
    | SetCurrentDate Date
    | Tick Time
    | ToggleSideBar


type alias Model =
    {
     activePage : Page
    , config : RemoteData String Config.Model.Model
    , currentDate : Date
    , errors : List Error
    , language : Language
    , offline : Bool
    , pageItem : ItemManager.Model.Model
    , sidebarOpen : Bool
    }


type alias Flags =
    { accessToken : String
    , hostname : String
    }


type Sidebar
    = Top
    | Left


emptyModel : Model
emptyModel =
    {
     activePage = Dashboard
    , config = NotAsked
    , currentDate = Date.fromTime 0
    , errors = []
    , language = English
    , offline = False
    , pageItem = ItemManager.Model.emptyModel
    , sidebarOpen = False
    }
