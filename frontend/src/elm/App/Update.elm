port module App.Update exposing (init, update, subscriptions)

import App.Model exposing (..)
import App.PageType exposing (Page(..))
import App.Utils exposing (handleErrors)
import Config
import Date
import Dict
import ItemManager.Model
import ItemManager.Update
import Json.Decode exposing (bool, decodeValue)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Time exposing (minute)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( config, cmds, activePage ) =
            case (Dict.get flags.hostname Config.configs) of
                Just config ->
                    let
                        defaultCmds =
                            [ Task.perform SetCurrentDate Date.now
                            ]

                        ( cmds, activePage_ ) =
                                (  defaultCmds
                                , emptyModel.activePage
                                )
                    in
                        ( Success config
                        , cmds
                        , activePage_
                        )

                Nothing ->
                    ( Failure "No config found"
                    , [ Cmd.none ]
                    , emptyModel.activePage
                    )
    in
        ( { emptyModel
            | activePage = activePage
            , config = config
          }
        , Cmd.batch cmds
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        backendUrl =
            case model.config of
                Success config ->
                    config.backendUrl

                _ ->
                    ""
    in
        case msg of
            HandleOfflineEvent (Ok offline) ->
                { model | offline = offline } ! []

            HandleOfflineEvent (Err err) ->
                model ! []


            MsgItemManager subMsg ->

                        let
                            ( val, cmds, redirectPage, maybeError ) =
                                ItemManager.Update.update model.currentDate backendUrl subMsg model.pageItem

                            modelUpdated =
                                { model | pageItem = val }

                            ( modelUpdatedWithSetPage, setPageCmds ) =
                                Maybe.map
                                    (\page ->
                                        update (SetActivePage page) modelUpdated
                                    )
                                    redirectPage
                                    |> Maybe.withDefault ( modelUpdated, Cmd.none )

                            modelUpdatedWithError =
                                handleErrors maybeError modelUpdatedWithSetPage
                        in
                            ( modelUpdatedWithError
                            , Cmd.batch
                                [ Cmd.map MsgItemManager cmds
                                , setPageCmds
                                ]
                            )



            NoOp ->
                model ! []


            SetActivePage page ->
                let

                    ( modelUpdated, command ) =
                        -- For a few, we also delegate some initialization
                        case page of
                            Dashboard ->
                                -- If we're showing a `Items` page, make sure we `Subscribe`
                                update (MsgItemManager ItemManager.Model.FetchAll) model

                            Item id ->
                                -- If we're showing a `Item`, make sure we `Subscribe`
                                update (MsgItemManager (ItemManager.Model.Subscribe id)) model

                            _ ->
                                ( model, Cmd.none )
                in
                    -- Close the sidebar in case it was opened.
                    ( { modelUpdated
                        | activePage = page
                        , sidebarOpen = False
                      }
                    , command
                    )

            SetCurrentDate date ->
                { model | currentDate = date } ! []

            Tick _ ->
                model ! [ Task.perform SetCurrentDate Date.now ]

            ToggleSideBar ->
                { model | sidebarOpen = not model.sidebarOpen } ! []



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
         Time.every minute Tick
        , offline (decodeValue bool >> HandleOfflineEvent)
        ]



{-| Get a singal if internet connection is lost.
-}
port offline : (Value -> msg) -> Sub msg

