port module Members.State exposing (update, subscriptions)

import Members.Types exposing (..)
import FormValidation
import Form.Validation exposing (..)
import Members.Rest exposing (..)
import Sum exposing (withSummaries)
import Types exposing (Model)
import Date
import Dict
import Json.Decode as JD
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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


validateMemberNameForm : MemberNameForm -> MemberNameForm
validateMemberNameForm form =
    let
        validators =
            [ .name >> FormValidation.validateNotBlank "name" ]

        errors =
            Dict.fromList <| FormValidation.validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| FormValidation.stringNotBlankResult form.name
        }


validatePaymentForm : PaymentForm -> PaymentForm
validatePaymentForm form =
    let
        validators =
            [ .amount >> FormValidation.validateFloat "amount" ]

        errors =
            Dict.fromList <| FormValidation.validateAll validators form
    in
        { form
            | errors = errors
            , result = Result.toMaybe <| String.toFloat form.amount
        }



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
