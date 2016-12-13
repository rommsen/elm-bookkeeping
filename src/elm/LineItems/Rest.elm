module LineItems.Rest exposing (..)

import LineItems.Types exposing (..)
import Json.Decode.Pipeline
import Json.Decode as JD
import Json.Encode as JE


lineItemDecoder : JD.Decoder LineItem
lineItemDecoder =
    Json.Decode.Pipeline.decode LineItem
        |> Json.Decode.Pipeline.required "id" (JD.string)
        |> Json.Decode.Pipeline.required "name" (JD.string)
        |> Json.Decode.Pipeline.required "amount" (JD.float)


lineItemEncoder : LineItem -> JE.Value
lineItemEncoder lineItem =
    JE.object
        [ ( "id", JE.string <| lineItem.id )
        , ( "name", JE.string <| lineItem.name )
        , ( "amount", JE.float <| lineItem.amount )
        ]
