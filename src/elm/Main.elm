module Main exposing (main)

import State exposing (init, subscriptions, update)
import Types exposing (Msg, Model)
import View exposing (view)
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
