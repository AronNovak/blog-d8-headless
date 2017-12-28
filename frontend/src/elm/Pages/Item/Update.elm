module Pages.Item.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Pages.Item.Model exposing (Msg(..))
import Item.Model exposing (Item)


update : BackendUrl -> Msg -> Item -> ( Item, Cmd Msg, Maybe Page )
update backendUrl msg item =
    case msg of

        SetRedirectPage page ->
            ( item, Cmd.none, Just page )
