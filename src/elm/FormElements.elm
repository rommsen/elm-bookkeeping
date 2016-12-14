module FormElements exposing (wrapFormElement)

import Html exposing (..)
import Html.Attributes exposing (..)


wrapFormElement : String -> Maybe String -> Html msg -> Html msg
wrapFormElement elementLabel elementError element =
    let
        withError =
            case elementError of
                Just error ->
                    [ element
                    , i [ class "fa fa-warning" ] []
                    , span [ class "help is-danger" ] [ text error ]
                    ]

                Nothing ->
                    [ element ]
    in
        div [ class "control" ]
            [ label [ class "label" ] [ text elementLabel ]
            , p
                [ classList
                    [ ( "control", True )
                    , ( "has-icon", elementError /= Nothing )
                    , ( "has-icon-right", elementError /= Nothing )
                    ]
                ]
                withError
            ]
