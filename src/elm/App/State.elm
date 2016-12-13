module App.State exposing (..)

import App.Types exposing (..)
import Types exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            { model | selectedTab = tab } ! []
