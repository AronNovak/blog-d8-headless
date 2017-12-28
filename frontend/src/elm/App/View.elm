module App.View exposing (..)

import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Config.View
import Error.View
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, src, style, target)
import Html.Events exposing (onClick)
import ItemManager.View exposing (..)
import Pages.PageNotFound.View exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (Language(English), translateText)
import Utils.Html exposing (emptyNode, showIf)


view : Model -> Html Msg
view model =
    case model.config of
        Failure err ->
            Config.View.view

        Success config ->
            case model.activePage of
                _ ->
                    let
                        mainAttributes =
                            if model.sidebarOpen then
                                [ class "pusher dimmed"
                                , onClick ToggleSideBar
                                ]
                            else
                                [ class "pusher"
                                ]

                        -- The errors Debug can always be in English.
                        debugErrors =
                            showIf config.debug <| Error.View.view English model.errors
                    in
                        div [ class "pushable" ]
                            [ -- Sidebar menu - responsive only
                              viewSidebar model Top
                            , div
                                mainAttributes
                                [ debugErrors
                                , div
                                    [ class "ui grid container" ]
                                    -- Non-responsive menu
                                    [ viewSidebar model Left
                                    , div
                                        [ class "ui main grid" ]
                                        [ viewTopMenu
                                        , viewMainContent model
                                        ]
                                    ]
                                ]
                            ]

        _ ->
            -- This should be instantaneous
            emptyNode


{-| Responsive top menu.
-}
viewTopMenu : Html Msg
viewTopMenu =
    div
        [ class "ui fixed inverted main menu" ]
        [ div
            [ class "ui container" ]
            [ a
                [ class "launch icon item sidebar-toggle"
                , onClick ToggleSideBar
                ]
                [ i [ class "sidebar icon" ] []
                ]
            ]
        ]


viewSidebar : Model -> Sidebar -> Html Msg
viewSidebar model sidebar =

            let
                wrapperClasses =
                    case sidebar of
                        Top ->
                            [ ( "ui sidebar inverted vertical menu", True )
                            , ( "visible", model.sidebarOpen )
                            ]

                        Left ->
                            [ ( "ui left fixed vertical inverted menu", True ) ]
            in
                div
                    [ classList wrapperClasses ]
                    [ a
                        [ class "item"
                        ]
                        []
                    , a
                        [ class "item"
                        , onClick <| SetActivePage Dashboard
                        ]
                        [ translateText model.language <| Trans.Sidebar Trans.Dashboard ]
                    , span
                        [ class "item"
                        ]
                        [ translateText model.language <|
                            Trans.Sidebar <|
                                if model.offline then
                                    Trans.NotConnected
                                else
                                    Trans.Connected
                        , i
                            [ classList
                                [ ( "icon wifi", True )
                                , ( "disabled", model.offline )
                                ]
                            ]
                            []
                        ]
                    , a
                        [ class "item"
                        ]
                        [ translateText model.language <| Trans.Sidebar Trans.SignOut ]
                    ]



viewMainContent : Model -> Html Msg
viewMainContent model =
    let
        viewContent =
            case model.activePage of
                AccessDenied ->
                    div [] [ translateText model.language <| Trans.AccessDenied ]

                PageNotFound ->
                    -- We don't need to pass any cmds, so we can call the view directly
                    Pages.PageNotFound.View.view model.language

                Dashboard ->

                            Html.map MsgItemManager <|
                                ItemManager.View.viewItems model.currentDate model.language model.pageItem



                Item id ->

                            Html.map MsgItemManager <|
                                ItemManager.View.viewPageItem model.currentDate model.language id model.pageItem


    in
                viewContent


{-| Get menu items classes. This function gets the active page and checks if
it is indeed the page used.
-}
classByPage : Page -> Page -> Attribute a
classByPage page activePage =
    classList
        [ ( "item", True )
        , ( "active", page == activePage )
        ]
