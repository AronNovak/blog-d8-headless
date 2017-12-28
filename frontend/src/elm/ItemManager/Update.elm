port module ItemManager.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict exposing (Dict)
import Error.Model exposing (Error)
import Error.Utils exposing (httpError, noError, plainError)
import HttpBuilder exposing (get, withQueryParams)
import Item.Model exposing (Item, ItemId)
import ItemManager.Decoder exposing (decodeItemFromResponse, decodeItemsFromResponse)
import ItemManager.Model exposing (..)
import ItemManager.Utils exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Pages.Item.Update
import Pages.Items.Update
import RemoteData exposing (RemoteData(..))
import Utils.WebData exposing (sendWithHandler)


update : Date -> BackendUrl -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page, Maybe Error )
update currentDate backendUrl msg model =
    case msg of
        Subscribe id ->
            case getItem id model of
                NotAsked ->
                    let
                        ( updatedModel, updatedCmds ) =
                            fetchItemFromBackend backendUrl id model
                    in
                        ( updatedModel
                        , updatedCmds
                        , Nothing
                        , noError
                        )

                Loading ->
                    ( model, Cmd.none, Nothing, noError )

                Failure _ ->
                    let
                        ( val, cmds ) =
                            fetchItemFromBackend backendUrl id model
                    in
                        ( val
                        , cmds
                        , Nothing
                        , noError
                        )

                Success _ ->
                    ( model, Cmd.none, Nothing, noError )

        Unsubscribe id ->
            ( { model | items = Dict.remove id model.items }
            , Cmd.none
            , Nothing
            , noError
            )

        FetchAll ->
            let
                ( val, cmds ) =
                    fetchAllItemsFromBackend backendUrl model
            in
                ( val, cmds, Nothing, noError )

        MsgPagesItem id subMsg ->
            case getItem id model of
                Success item ->
                    let
                        ( subModel, subCmd, redirectPage ) =
                            Pages.Item.Update.update backendUrl subMsg item
                    in
                        ( { model | items = Dict.insert id (Success subModel) model.items }
                        , Cmd.map (MsgPagesItem id) subCmd
                        , redirectPage
                        , noError
                        )

                _ ->
                    -- We've received a message for a Item which we either
                    -- aren't subscribed to, or dont' have initial data for yet.
                    -- This normally wouldn't happen, though we may needd to think
                    -- about synchronization between obtaining our initial data and
                    -- possible "pusher" messages. (Could pusher messages sometimes
                    -- arrive before the initial data, and if so, should we ignore
                    -- them or queue them up? We may need server timestamps on the initial
                    -- data and the pusher messages to know.)
                    ( model, Cmd.none, Nothing, noError )

        MsgPagesItems subMsg ->
            let
                ( subModel, subCmd, redirectPage ) =
                    Pages.Items.Update.update backendUrl subMsg (unwrapItemsDict model.items) model.itemsPage
            in
                ( { model | itemsPage = subModel }
                , Cmd.map MsgPagesItems subCmd
                , redirectPage
                , noError
                )

        HandleFetchedItems (Ok items) ->
            ( { model | items = wrapItemsDict items }
            , Cmd.none
            , Nothing
            , noError
            )

        HandleFetchedItems (Err error) ->
            ( model
            , Cmd.none
            , Nothing
            , httpError "ItemManager.Update" "HandleFetchedItems" error
            )

        HandleFetchedItem itemId (Ok item) ->
            let
                -- Let Item settings fetch own data.
                -- @todo: Pass the activePage here, so we can fetch
                -- data only when really needed.
                updatedModel =
                    { model | items = Dict.insert itemId (Success item) model.items }
            in
                ( updatedModel
                , Cmd.none
                , Nothing
                , noError
                )

        HandleFetchedItem itemId (Err error) ->
            ( { model | items = Dict.insert itemId (Failure error) model.items }
            , Cmd.none
            , Nothing
            , httpError "ItemManager.Update" "HandleFetchedItem" error
            )


fetchItemFromBackend : BackendUrl -> ItemId -> Model -> ( Model, Cmd Msg )
fetchItemFromBackend backendUrl itemId model =
    let
        command =
            HttpBuilder.get (backendUrl ++ "/api/items/" ++ itemId)
                |> sendWithHandler decodeItemFromResponse (HandleFetchedItem itemId)
    in
        ( { model | items = Dict.insert itemId Loading model.items }
        , command
        )


fetchAllItemsFromBackend : BackendUrl ->  Model -> ( Model, Cmd Msg )
fetchAllItemsFromBackend backendUrl model =
    let
        command =
            HttpBuilder.get (backendUrl ++ "/api/items")
                |> sendWithHandler decodeItemsFromResponse HandleFetchedItems
    in
        ( model
        , command
        )


