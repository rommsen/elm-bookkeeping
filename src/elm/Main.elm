module Main exposing (main)

import State exposing (init, subscriptions, appUpdate, lineItemUpdate, memberUpdate)
import Types exposing (..)
import View exposing (view)
import Html


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( newModel, cmd ) =
                    appUpdate appMsg model
            in
                ( newModel, Cmd.map AppMsg cmd )

        MemberMsg memberMsg ->
            let
                ( newModel, cmd ) =
                    memberUpdate memberMsg model
            in
                ( newModel, Cmd.map MemberMsg cmd )

        LineItemMsg lineItemMsg ->
            let
                ( newModel, cmd ) =
                    lineItemUpdate lineItemMsg model
            in
                ( newModel, Cmd.map LineItemMsg cmd )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
