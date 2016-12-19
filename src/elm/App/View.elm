module App.View exposing (viewHeader)

import App.Types exposing (..)
import Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewHeader : Model -> Html Msg
viewHeader model =
    section [ class "hero is-info" ]
        [ div [ class "hero-head" ]
            [ div [ class "container" ]
                [ div [ class "nav" ]
                    [ div [ class "nav-left" ]
                        [ span [ class "nav-item is-brand" ] [ text "Bookkeeping" ] ]
                    ]
                ]
            ]
        , div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column" ]
                        [ div [ class "box" ]
                            [ div
                                [ classList
                                    [ ( "notification", True )
                                    , ( "is-danger", model.totalBalance < 0 )
                                    , ( "is-success", model.totalBalance >= 0 )
                                    ]
                                ]
                                [ p [ class "title is-4" ]
                                    [ text <| "Balance:  " ++ toString model.totalBalance ++ "€" ]
                                ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box" ]
                            [ div
                                [ classList
                                    [ ( "notification", True )
                                    , ( "is-danger", model.memberDebitTotal < 0 )
                                    , ( "is-success", model.memberDebitTotal >= 0 )
                                    ]
                                ]
                                [ p [ class "title is-4" ]
                                    [ text <| "Members:  " ++ toString model.memberDebitTotal ++ "€" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "hero-foot" ]
            [ div [ class "container" ]
                [ nav [ class "tabs is-boxed is-medium" ]
                    [ ul []
                        [ li
                            [ classList [ ( "is-active", model.selectedTab == MemberTab ) ]
                            , onClick <| SelectTab MemberTab
                            ]
                            [ a []
                                [ span [ class "icon" ] [ i [ class "fa fa-users" ] [] ]
                                , span [] [ text "Members" ]
                                ]
                            ]
                        , li
                            [ classList [ ( "is-active", model.selectedTab == LineItemTab ) ]
                            , onClick <| SelectTab LineItemTab
                            ]
                            [ a []
                                [ span [ class "icon" ] [ i [ class "fa fa-list" ] [] ]
                                , span [] [ text "Line Items" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
