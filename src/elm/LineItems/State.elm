port module LineItems.State exposing (update, subscriptions)

import LineItems.Types exposing (..)
import FormValidation
import LineItems.Rest exposing (..)
import Types exposing (Model)
import Sum exposing (withSummaries)
import Dict
import Json.Decode as JD


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectLineItem lineItem ->
            { model | lineItem = Just lineItem, lineItemForm = lineItemFormFromLineItem lineItem } ! []

        CancelLineItem ->
            { model | lineItem = Nothing, lineItemForm = emptyLineItemForm } ! []

        InputLineItemName name ->
            let
                form =
                    model.lineItemForm
            in
                { model | lineItemForm = validateLineItemForm { form | name = name } }
                    ! []

        InputLineItemAmount amount ->
            let
                form =
                    model.lineItemForm
            in
                { model | lineItemForm = validateLineItemForm { form | amount = amount } }
                    ! []

        LineItemAdded value ->
            case JD.decodeValue lineItemDecoder value of
                Ok lineItem ->
                    withSummaries
                        { model | lineItems = lineItem :: model.lineItems, lineItemForm = emptyLineItemForm }
                        ! []

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        model ! []

        LineItemUpdated value ->
            case JD.decodeValue lineItemDecoder value of
                Ok newLineItem ->
                    let
                        map lineItem =
                            if lineItem.id == newLineItem.id then
                                newLineItem
                            else
                                lineItem
                    in
                        withSummaries
                            { model
                                | lineItems = List.map map model.lineItems
                                , lineItem = Just newLineItem
                            }
                            ! []

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        model ! []

        SaveLineItem ->
            let
                lineItemForm =
                    validateLineItemForm model.lineItemForm
            in
                case lineItemForm.result of
                    Just result ->
                        case model.lineItem of
                            Just lineItem ->
                                ( model, updateLineItemCmd { lineItem | name = result.name, amount = result.amount } )

                            Nothing ->
                                ( model, addLineItem <| lineItemEncoder <| newLineItem result.name result.amount )

                    Nothing ->
                        { model | lineItemForm = lineItemForm } ! []

        DeleteLineItem lineItem ->
            ( model, deleteLineItem <| lineItemEncoder lineItem )

        LineItemDeleted value ->
            case JD.decodeValue lineItemDecoder value of
                Ok lineItem ->
                    withSummaries
                        { model | lineItems = deleteLineItemFromList lineItem model.lineItems }
                        ! []

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        model ! []


updateLineItemCmd : LineItem -> Cmd msg
updateLineItemCmd =
    lineItemEncoder >> updateLineItem


deleteLineItemFromList : LineItem -> List LineItem -> List LineItem
deleteLineItemFromList lineItem list =
    List.filter (\{ id } -> id /= lineItem.id) list


validateLineItemForm : LineItemForm -> LineItemForm
validateLineItemForm form =
    let
        validators =
            [ .amount >> FormValidation.validateFloat "amount"
            , .name >> FormValidation.validateNotBlank "name"
            ]

        errors =
            Dict.fromList <| FormValidation.validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| Result.map2 (LineItem "") (FormValidation.stringNotBlankResult form.name) (String.toFloat form.amount)
        }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ lineItemAdded LineItemAdded
        , lineItemUpdated LineItemUpdated
        , lineItemDeleted LineItemDeleted
        ]


port addLineItem : JD.Value -> Cmd msg


port updateLineItem : JD.Value -> Cmd msg


port deleteLineItem : JD.Value -> Cmd msg


port lineItemAdded : (JD.Value -> msg) -> Sub msg


port lineItemUpdated : (JD.Value -> msg) -> Sub msg


port lineItemDeleted : (JD.Value -> msg) -> Sub msg
