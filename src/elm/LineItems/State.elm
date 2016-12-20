port module LineItems.State exposing (init, update, subscriptions)

import LineItems.Types exposing (..)
import Form.Validation exposing (..)
import LineItems.Rest exposing (..)
import Sum exposing (sumAmount)
import Json.Decode as JD


initialModel : Model
initialModel =
    { lineItems = []
    , lineItem = Nothing
    , lineItemForm = emptyLineItemForm
    , lineItemTotal = 0
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


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
                { model
                    | lineItemForm = { form | name = name, errors = validateLineItemForm form }
                }
                    ! []

        InputLineItemAmount amount ->
            let
                form =
                    model.lineItemForm
            in
                { model
                    | lineItemForm = { form | amount = amount, errors = validateLineItemForm form }
                }
                    ! []

        LineItemAdded value ->
            case JD.decodeValue lineItemDecoder value of
                Ok lineItem ->
                    withSummaries
                        { model
                            | lineItems = lineItem :: model.lineItems
                            , lineItemForm = emptyLineItemForm
                        }
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
                                , lineItemForm = emptyLineItemForm
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
                form =
                    model.lineItemForm

                lineItemForm =
                    { form | errors = validateLineItemForm form }
            in
                case extractLineItemFromForm lineItemForm of
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


validateLineItemForm : LineItemForm -> List Error
validateLineItemForm form =
    begin form
        |> validate (validateFloat "amount" << .amount)
        |> validate (validateNotBlank "name" << .name)
        |> extractErrors


extractLineItemFromForm : LineItemForm -> Maybe LineItem
extractLineItemFromForm form =
    Result.map2 (LineItem "") (Form.Validation.stringNotBlankResult form.name) (String.toFloat form.amount)
        |> Result.toMaybe


withSummaries : Model -> Model
withSummaries model =
    { model | lineItemTotal = sumAmount .lineItems model }



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
