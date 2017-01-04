port module Members.State exposing (init, update, subscriptions, memberBalance)

import Members.Types exposing (..)
import Form.Validation exposing (..)
import Sum exposing (..)
import Members.Rest exposing (..)
import Sum exposing (sumAmount)
import Date
import Json.Decode as JD
import Task


initialModel : Model
initialModel =
    { members = []
    , member = Nothing
    , memberNameForm = emptyMemberNameForm
    , memberDebitTotal = 0
    , memberPaymentsTotal = 0
    , paymentForm = emptyPaymentForm
    , monthForm = emptyMonthForm
    , memberPane = MemberPaneShowNone
    , memberFilter = MemberFilterAll
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMemberName name ->
            let
                form =
                    model.memberNameForm
            in
                { model
                    | memberNameForm = { form | name = name, errors = validateMemberNameForm form }
                }
                    ! []

        SaveMemberName ->
            let
                form =
                    model.memberNameForm

                memberNameForm =
                    { form | errors = validateMemberNameForm form }
            in
                case extractNameFromMemberNameForm memberNameForm of
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
                    model.paymentForm

                paymentForm =
                    { form | errors = validatePaymentForm form }
            in
                case extractAmountFromPaymentForm paymentForm of
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
                { model
                    | paymentForm = { form | amount = amount, errors = validatePaymentForm form }
                }
                    ! []

        SelectMonth newMonth ->
            let
                form =
                    model.monthForm
            in
                { model
                    | monthForm = { form | month = newMonth, errors = validateMonthForm form }
                }
                    ! []

        InputMonthYear year ->
            let
                form =
                    model.monthForm
            in
                { model
                    | monthForm = { form | year = year, errors = validateMonthForm form }
                }
                    ! []

        InputMonthAmount amount ->
            let
                form =
                    model.monthForm
            in
                { model
                    | monthForm = { form | amount = amount, errors = validateMonthForm form }
                }
                    ! []

        AddMonthToActiveMembers ->
            let
                form =
                    model.monthForm

                monthForm =
                    { form | errors = validateMonthForm form }
            in
                case extractMonthFromForm monthForm of
                    Just result ->
                        let
                            cmds =
                                model.members
                                    |> List.filter (\month -> month.active == True && not (memberHasMonth month result))
                                    |> List.map (\month -> { month | months = result :: month.months })
                                    |> List.map updateMemberCmd
                        in
                            { model | monthForm = monthForm } ! cmds

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

        Login value ->
            let
                _ =
                    Debug.log "login" value
            in
                model ! []


updateMemberCmd : Member -> Cmd msg
updateMemberCmd =
    memberEncoder >> updateMember


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteFromList : a -> List a -> List a
deleteFromList toDelete list =
    List.filter ((/=) toDelete) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    a == b


validateMonthForm : MonthForm -> List Error
validateMonthForm form =
    begin form
        |> validate (validateFloat "amount" << .amount)
        |> validate (validateFloat "year" << .year)
        |> extractErrors


extractMonthFromForm : MonthForm -> Maybe Month
extractMonthFromForm form =
    Result.map2 (Month form.month) (String.toInt form.year) (String.toFloat form.amount)
        |> Result.toMaybe


validatePaymentForm : PaymentForm -> List Error
validatePaymentForm form =
    begin form
        |> validate (validateFloat "amount" << .amount)
        |> extractErrors


extractAmountFromPaymentForm : PaymentForm -> Maybe Float
extractAmountFromPaymentForm form =
    String.toFloat form.amount |> Result.toMaybe


validateMemberNameForm : MemberNameForm -> List Error
validateMemberNameForm form =
    begin form
        |> validate (validateNotBlank "name" << .name)
        |> extractErrors


extractNameFromMemberNameForm : MemberNameForm -> Maybe String
extractNameFromMemberNameForm form =
    stringNotBlankResult form.name |> Result.toMaybe


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
    in
        { model
            | memberPaymentsTotal = memberPaymentsTotal
            , memberDebitTotal = memberPaymentsTotal - memberDebitTotal
        }


sumMemberPayment : Member -> Float
sumMemberPayment member =
    sumAmount .payments member


sumMemberDebit : Member -> Float
sumMemberDebit member =
    sumAmount .months member


memberBalance : Member -> Float
memberBalance member =
    sumMemberPayment member - sumMemberDebit member



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ memberAdded MemberAdded
        , memberUpdated MemberUpdated
        ]


port addMember : JD.Value -> Cmd msg


port updateMember : JD.Value -> Cmd msg


port memberAdded : (JD.Value -> msg) -> Sub msg


port memberUpdated : (JD.Value -> msg) -> Sub msg
