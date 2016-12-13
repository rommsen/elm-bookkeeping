port module State exposing (init, subscriptions, appUpdate, memberUpdate, lineItemUpdate)

import Rest exposing (..)
import Types exposing (..)
import Date
import Dict
import Json.Decode as JD
import Task


initialModel : Model
initialModel =
    { members = []
    , member = Nothing
    , memberNameForm = emptyMemberNameForm
    , paymentForm = emptyPaymentForm
    , monthForm = emptyMonthForm
    , lineItems = []
    , lineItem = Nothing
    , lineItemForm = emptyLineItemForm
    , totalBalance = 0
    , totalMemberDebit = 0
    , memberPane = MemberPaneShowNone
    , memberFilter = MemberFilterAll
    , selectedTab = MemberTab
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


appUpdate : AppMsg -> Model -> ( Model, Cmd AppMsg )
appUpdate msg model =
    case msg of
        SelectTab tab ->
            { model | selectedTab = tab } ! []


memberUpdate : MemberMsg -> Model -> ( Model, Cmd MemberMsg )
memberUpdate msg model =
    case msg of
        InputMemberName name ->
            let
                form =
                    model.memberNameForm
            in
                { model | memberNameForm = validateMemberNameForm { form | name = name } } ! []

        SaveMemberName ->
            let
                form =
                    validateMemberNameForm model.memberNameForm
            in
                case form.result of
                    Just result ->
                        case model.member of
                            Just member ->
                                ( model, updateMemberCmd { member | name = model.memberNameForm.name } )

                            Nothing ->
                                ( model, addMember <| memberEncoder <| memberWithName model.memberNameForm.name )

                    Nothing ->
                        { model | memberNameForm = form } ! []

        MemberAdded value ->
            case JD.decodeValue memberDecoder value of
                Ok newMember ->
                    withSummaries
                        { model
                            | members = newMember :: model.members
                            , memberPane = MemberPaneShowNone
                            , member = Nothing
                        }
                        ! []

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        model ! []

        MemberUpdated value ->
            case JD.decodeValue memberDecoder value of
                Ok newMember ->
                    let
                        map member =
                            if member.id == newMember.id then
                                newMember
                            else
                                member
                    in
                        withSummaries
                            { model
                                | members = List.map map model.members
                                , member = Just newMember
                                , memberNameForm = memberNameFormFromMember newMember
                            }
                            ! []

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        model ! []

        ToggleMemberIsActive member ->
            ( model, updateMemberCmd { member | active = not <| member.active } )

        CreateMemberPayment ->
            let
                form =
                    validatePaymentForm model.paymentForm
            in
                case form.result of
                    Just result ->
                        case model.member of
                            Just member ->
                                ( { model | paymentForm = emptyPaymentForm }
                                , Date.now
                                    |> Task.map (\date -> Payment result date)
                                    |> Task.perform (SaveMemberPayment member)
                                )

                            Nothing ->
                                model ! []

                    Nothing ->
                        { model | paymentForm = form } ! []

        SaveMemberPayment member payment ->
            ( model, updateMemberCmd { member | payments = payment :: member.payments } )

        DeleteMemberPayment member payment ->
            ( model, updateMemberCmd { member | payments = deleteFromList payment member.payments } )

        InputMemberPaymentAmount amount ->
            let
                form =
                    model.paymentForm
            in
                { model | paymentForm = validatePaymentForm { form | amount = amount } }
                    ! []

        SelectMonth newMonth ->
            let
                form =
                    model.monthForm
            in
                { model | monthForm = validateMonthForm { form | month = newMonth } } ! []

        InputMonthYear year ->
            let
                form =
                    model.monthForm
            in
                { model | monthForm = validateMonthForm { form | year = year } } ! []

        InputMonthAmount amount ->
            let
                form =
                    model.monthForm
            in
                { model | monthForm = validateMonthForm { form | amount = amount } } ! []

        AddMonthToActiveMembers ->
            let
                monthForm =
                    validateMonthForm model.monthForm
            in
                case monthForm.result of
                    Just result ->
                        let
                            cmds =
                                model.members
                                    |> List.filter (\month -> month.active == True && not (memberHasMonth month result))
                                    |> List.map (\month -> { month | months = result :: month.months })
                                    |> List.map updateMemberCmd
                        in
                            model ! cmds

                    Nothing ->
                        { model | monthForm = monthForm } ! []

        DeleteMonthFromMember month member ->
            ( model, updateMemberCmd { member | months = deleteFromList month member.months } )

        ChangeMemberPane memberPane ->
            case memberPane of
                MemberPaneShowDetails member ->
                    { model
                        | memberPane = memberPane
                        , member = Just member
                        , memberNameForm = memberNameFormFromMember member
                    }
                        ! []

                _ ->
                    { model
                        | memberPane = memberPane
                        , member = Nothing
                        , memberNameForm = emptyMemberNameForm
                    }
                        ! []

        FilterMembers memberFilter ->
            { model | memberFilter = memberFilter } ! []


lineItemUpdate : LineItemMsg -> Model -> ( Model, Cmd LineItemMsg )
lineItemUpdate msg model =
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


updateMemberCmd : Member -> Cmd msg
updateMemberCmd =
    memberEncoder >> updateMember


updateLineItemCmd : LineItem -> Cmd msg
updateLineItemCmd =
    lineItemEncoder >> updateLineItem


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteFromList : a -> List a -> List a
deleteFromList toDelete list =
    List.filter ((/=) toDelete) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    a == b


deleteLineItemFromList : LineItem -> List LineItem -> List LineItem
deleteLineItemFromList lineItem list =
    List.filter (\{ id } -> id /= lineItem.id) list


withSummaries : Model -> Model
withSummaries model =
    let
        memberPaymentsTotal =
            model.members
                |> List.map sumMemberPayment
                |> List.sum

        memberDebitTotal =
            model.members
                |> List.map sumMemberDebit
                |> List.sum

        lineItemTotal =
            sumAmount .lineItems model
    in
        { model
            | totalBalance = memberPaymentsTotal + lineItemTotal
            , totalMemberDebit = memberPaymentsTotal - memberDebitTotal
        }


validateMonthForm : MonthForm -> MonthForm
validateMonthForm form =
    let
        validators =
            [ .amount >> validateFloat "amount"
            , .year >> validateInt "year"
            ]
    in
        { form
            | errors = Dict.fromList <| validateAll validators form
            , result = Result.toMaybe <| Result.map2 (Month form.month) (String.toInt form.year) (String.toFloat form.amount)
        }


validateMemberNameForm : MemberNameForm -> MemberNameForm
validateMemberNameForm form =
    let
        validators =
            [ .name >> validateNotBlank "name" ]

        errors =
            Dict.fromList <| validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| stringNotBlankResult form.name
        }


validatePaymentForm : PaymentForm -> PaymentForm
validatePaymentForm form =
    let
        validators =
            [ .amount >> validateFloat "amount" ]

        errors =
            Dict.fromList <| validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| String.toFloat form.amount
        }


validateLineItemForm : LineItemForm -> LineItemForm
validateLineItemForm form =
    let
        validators =
            [ .amount >> validateFloat "amount"
            , .name >> validateNotBlank "name"
            ]

        errors =
            Dict.fromList <| validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| Result.map2 (LineItem "") (stringNotBlankResult form.name) (String.toFloat form.amount)
        }


validate : (a -> b) -> a -> b
validate validator record =
    validator record


validateAll : List (a -> b) -> a -> List b
validateAll validators record =
    List.map (\validator -> validator record) validators


validateNotBlank : String -> String -> FormError
validateNotBlank name string =
    case stringNotBlankResult string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This should not be empty" )


stringNotBlankResult : String -> Result String String
stringNotBlankResult string =
    if String.isEmpty string then
        Err "string is empty"
    else
        Ok string


validateFloat : String -> String -> FormError
validateFloat name string =
    case String.toFloat string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This is not a valid number" )


validateInt : String -> String -> FormError
validateInt name string =
    case String.toInt string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This is not a valid number" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MemberMsg (memberAdded MemberAdded)
        , Sub.map MemberMsg (memberUpdated MemberUpdated)
        , Sub.map LineItemMsg (lineItemAdded LineItemAdded)
        , Sub.map LineItemMsg (lineItemUpdated LineItemUpdated)
        , Sub.map LineItemMsg (lineItemDeleted LineItemDeleted)
        ]


port addMember : JD.Value -> Cmd msg


port updateMember : JD.Value -> Cmd msg


port memberAdded : (JD.Value -> msg) -> Sub msg


port memberUpdated : (JD.Value -> msg) -> Sub msg


port addLineItem : JD.Value -> Cmd msg


port updateLineItem : JD.Value -> Cmd msg


port deleteLineItem : JD.Value -> Cmd msg


port lineItemAdded : (JD.Value -> msg) -> Sub msg


port lineItemUpdated : (JD.Value -> msg) -> Sub msg


port lineItemDeleted : (JD.Value -> msg) -> Sub msg
