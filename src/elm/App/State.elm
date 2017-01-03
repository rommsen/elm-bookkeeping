module App.State exposing (..)

import App.Types exposing (..)


initialModel : Model
initialModel =
    { selectedTab = MemberTab
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            { model | selectedTab = tab } ! []



-- Login tab ->
--     { model | selectedTab = tab } ! []
