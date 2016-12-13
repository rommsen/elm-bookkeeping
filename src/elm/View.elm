module View exposing (view)

import App.Types exposing (Tab(..))
import App.View
import LineItems.View
import Members.View
import Types exposing (Model, Msg(..))
import Html exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ Html.map AppMsg (App.View.viewHeader model)
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        MemberTab ->
            Html.map MemberMsg (Members.View.view model)

        LineItemTab ->
            Html.map LineItemMsg (LineItems.View.view model)
