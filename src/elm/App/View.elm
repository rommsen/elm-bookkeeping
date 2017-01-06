module App.View exposing (viewHeader, viewLogin)

import App.Types exposing (..)
import Types
import FormElements exposing (wrapFormElement)
import Form.Validation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewHeader : Types.Model -> Html Msg
viewHeader model =
    section [ class "hero is-info" ]
        [ div [ class "hero-head" ]
            [ div [ class "container" ]
                [ div [ class "nav" ]
                    [ div [ class "nav-left" ]
                        [ span [ class "nav-item is-brand" ] [ text "Bookkeeping" ] ]
                    , div
                        [ class "nav-center" ]
                        [ span
                            [ class "nav-item" ]
                            [ a
                                [ class "button is-info"
                                , onClick Logout
                                ]
                                [ span
                                    [ class "icon" ]
                                    [ i [ class "fa fa-sign-out" ] [] ]
                                ]
                            ]
                        ]
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
                                    , ( "is-danger", model.members.memberDebitTotal < 0 )
                                    , ( "is-success", model.members.memberDebitTotal >= 0 )
                                    ]
                                ]
                                [ p [ class "title is-4" ]
                                    [ text <| "Members:  " ++ toString model.members.memberDebitTotal ++ "€" ]
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
                            [ classList [ ( "is-active", model.app.selectedTab == MemberTab ) ]
                            , onClick <| SelectTab MemberTab
                            ]
                            [ a []
                                [ span [ class "icon" ] [ i [ class "fa fa-users" ] [] ]
                                , span [] [ text "Members" ]
                                ]
                            ]
                        , li
                            [ classList [ ( "is-active", model.app.selectedTab == LineItemTab ) ]
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


viewLogin : Model -> Html Msg
viewLogin model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "tile is-ancestor" ]
                [ div
                    [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ h1 [ class "title" ] [ text "Login" ]
                        , loginForm model.loginForm
                        ]
                    ]
                ]
            ]
        ]


loginForm : LoginForm -> Html Msg
loginForm form =
    let
        emailError =
            findError "email" form.errors

        passwordError =
            findError "password" form.errors

        emailInput =
            wrapFormElement "Email" emailError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", emailError /= Nothing )
                        ]
                    , onInput InputEmail
                    , placeholder "Email"
                    , value form.email
                    ]
                    []

        passwordInput =
            wrapFormElement "password" passwordError <|
                input
                    [ type_ "password"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", passwordError /= Nothing )
                        ]
                    , onInput InputPassword
                    , placeholder "Password"
                    , value form.password
                    ]
                    []

        submitButton =
            button
                [ type_ "submit"
                , class "button is-primary"
                ]
                [ text "Login" ]
    in
        Html.form [ onSubmit Login ]
            [ emailInput
            , passwordInput
            , div [ class "control is-grouped" ]
                [ submitButton ]
            ]
