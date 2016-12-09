module Rest exposing (..)

import Types exposing (..)
import Date
import Date.Extra.Format
import Html.Events exposing (targetValue)
import Json.Decode.Pipeline
import Json.Decode as JD
import Json.Encode as JE


dateDecoder : JD.Decoder Date.Date
dateDecoder =
    JD.string
        |> JD.andThen dateDecoderHelper


dateDecoderHelper : String -> JD.Decoder Date.Date
dateDecoderHelper string =
    case Date.fromString string of
        Ok date ->
            JD.succeed date

        Err err ->
            JD.fail err


monthSelectDecoder : JD.Decoder Date.Month
monthSelectDecoder =
    targetValue
        |> JD.andThen monthDecoder


monthStringDecoder : JD.Decoder Date.Month
monthStringDecoder =
    JD.string
        |> JD.andThen monthDecoder


monthDecoder : String -> JD.Decoder Date.Month
monthDecoder string =
    case string of
        "Jan" ->
            JD.succeed Date.Jan

        "Feb" ->
            JD.succeed Date.Feb

        "Mar" ->
            JD.succeed Date.Mar

        "Apr" ->
            JD.succeed Date.Apr

        "May" ->
            JD.succeed Date.May

        "Jun" ->
            JD.succeed Date.Jun

        "Jul" ->
            JD.succeed Date.Jul

        "Aug" ->
            JD.succeed Date.Aug

        "Sep" ->
            JD.succeed Date.Sep

        "Oct" ->
            JD.succeed Date.Oct

        "Nov" ->
            JD.succeed Date.Nov

        "Dec" ->
            JD.succeed Date.Dec

        _ ->
            JD.fail "month not available"


memberDecoder : JD.Decoder Member
memberDecoder =
    Json.Decode.Pipeline.decode Member
        |> Json.Decode.Pipeline.required "id" (JD.string)
        |> Json.Decode.Pipeline.required "name" (JD.string)
        |> Json.Decode.Pipeline.required "active" (JD.bool)
        |> Json.Decode.Pipeline.optional "months" (JD.list monthTypeDecoder) []
        |> Json.Decode.Pipeline.optional "payments" (JD.list paymentDecoder) []


monthTypeDecoder : JD.Decoder Month
monthTypeDecoder =
    Json.Decode.Pipeline.decode Month
        |> Json.Decode.Pipeline.required "month" (monthStringDecoder)
        |> Json.Decode.Pipeline.required "year" (JD.int)
        |> Json.Decode.Pipeline.required "amount" (JD.float)


paymentDecoder : JD.Decoder Payment
paymentDecoder =
    Json.Decode.Pipeline.decode Payment
        |> Json.Decode.Pipeline.required "amount" (JD.float)
        |> Json.Decode.Pipeline.required "added" (dateDecoder)


lineItemDecoder : JD.Decoder LineItem
lineItemDecoder =
    Json.Decode.Pipeline.decode LineItem
        |> Json.Decode.Pipeline.required "id" (JD.string)
        |> Json.Decode.Pipeline.required "name" (JD.string)
        |> Json.Decode.Pipeline.required "amount" (JD.float)


monthEncoder : Month -> JE.Value
monthEncoder month =
    JE.object
        [ ( "month", JE.string <| toString month.month )
        , ( "year", JE.int month.year )
        , ( "amount", JE.float month.amount )
        ]


paymentEncoder : Payment -> JE.Value
paymentEncoder payment =
    JE.object
        [ ( "amount", JE.float <| payment.amount )
        , ( "added", JE.string <| Date.Extra.Format.isoString payment.added )
        ]


memberEncoder : Member -> JE.Value
memberEncoder member =
    JE.object
        [ ( "id", JE.string <| member.id )
        , ( "name", JE.string <| member.name )
        , ( "active", JE.bool <| member.active )
        , ( "months", JE.list <| List.map monthEncoder <| member.months )
        , ( "payments", JE.list <| List.map paymentEncoder <| member.payments )
        ]


lineItemEncoder : LineItem -> JE.Value
lineItemEncoder lineItem =
    JE.object
        [ ( "id", JE.string <| lineItem.id )
        , ( "name", JE.string <| lineItem.name )
        , ( "amount", JE.float <| lineItem.amount )
        ]
