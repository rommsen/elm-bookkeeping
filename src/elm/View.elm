module View exposing (view)

import App.Types exposing (Tab(..))
import App.View
import LineItems.View
import Members.View
import Types exposing (Model, Msg(..))
import Html exposing (..)


view : Model -> Html Msg
view model =
    if model.app.loggedIn then
        div []
            [ Html.map AppMsg (App.View.viewHeader model)
            , viewBody model
            ]
    else
        div []
            [ Html.map AppMsg (App.View.viewLogin model.app) ]


viewBody : Model -> Html Msg
viewBody model =
    case model.app.selectedTab of
        MemberTab ->
            Html.map MemberMsg (Members.View.view model.members)

        LineItemTab ->
            Html.map LineItemMsg (LineItems.View.view model.lineItems)
