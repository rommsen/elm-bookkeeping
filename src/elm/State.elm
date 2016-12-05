port module State exposing (init, subscriptions, update)

import Rest exposing (..)
import Types exposing (..)
import Date
import Json.Decode as JD
import Task


initialModel : Model
initialModel =
    { members = []
    , member = Nothing
    , memberName = ""
    , month = Month Date.Jan 2016 7.5
    , memberPayment = 0
    , lineItems = []
    , lineItem = Nothing
    , lineItemName = ""
    , lineItemAmount = 0
    , totalBalance = 0
    , totalMemberDebit = 0
    , memberPane = MemberPaneShowNone
    , memberFilter = MemberFilterAll
    , selectedTab = 0
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMemberName name ->
            { model | memberName = name } ! []

        SaveMemberName ->
            if (String.isEmpty model.memberName) then
                model ! []
            else
                case model.member of
                    Just member ->
                        ( model, updateMemberCmd { member | name = model.memberName } )

                    Nothing ->
                        ( model, addMember <| memberEncoder <| memberWithName model.memberName )

        MemberAdded value ->
            case JD.decodeValue memberDecoder value of
                Ok member ->
                    { model | members = member :: model.members } ! []

                Err err ->
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

                        newModel =
                            withSummaries <|
                                { model
                                    | members = List.map map model.members
                                    , member = Just newMember
                                    , memberName = newMember.name
                                }
                    in
                        newModel ! []

                Err err ->
                    model ! []

        ToggleMemberIsActive member ->
            ( model, updateMemberCmd { member | active = not <| member.active } )

        CreateMemberPayment ->
            case model.member of
                Just member ->
                    ( { model | memberPayment = 0 }
                    , Date.now
                        |> Task.map (\date -> Payment model.memberPayment date)
                        |> Task.perform (SaveMemberPayment member)
                    )

                Nothing ->
                    model ! []

        SaveMemberPayment member payment ->
            ( model, updateMemberCmd { member | payments = payment :: member.payments } )

        DeleteMemberPayment member payment ->
            ( model, updateMemberCmd { member | payments = deleteFromList payment member.payments } )

        InputMemberPaymentAmount amount ->
            case String.toFloat amount of
                Ok val ->
                    { model | memberPayment = val } ! []

                Err bla ->
                    model ! []

        SelectMonth newMonth ->
            case model of
                { month } ->
                    { model | month = { month | month = newMonth } } ! []

        InputMonthYear year ->
            case ( String.toInt year, model ) of
                ( Ok val, { month } ) ->
                    { model | month = { month | year = val } } ! []

                ( Err bla, _ ) ->
                    model ! []

        InputMonthAmount amount ->
            case ( String.toFloat amount, model ) of
                ( Ok val, { month } ) ->
                    { model | month = { month | amount = val } } ! []

                ( Err bla, _ ) ->
                    model ! []

        AddMonthToActiveMembers ->
            let
                cmds =
                    model.members
                        |> List.filter (\m -> m.active == True && not (memberHasMonth m model.month))
                        |> List.map (\m -> { m | months = model.month :: m.months })
                        |> List.map updateMemberCmd
            in
                model ! cmds

        DeleteMonthFromMember month member ->
            ( model, updateMemberCmd { member | months = deleteFromList month member.months } )

        SelectLineItem lineItem ->
            { model | lineItem = Just lineItem, lineItemName = lineItem.name, lineItemAmount = lineItem.amount } ! []

        CancelLineItem ->
            { model | lineItem = Nothing, lineItemName = "", lineItemAmount = 0 } ! []

        InputLineItemName name ->
            { model | lineItemName = name } ! []

        InputLineItemAmount amount ->
            case String.toFloat amount of
                Ok val ->
                    { model | lineItemAmount = val } ! []

                Err bla ->
                    model ! []

        LineItemAdded value ->
            case JD.decodeValue lineItemDecoder value of
                Ok lineItem ->
                    withSummaries { model | lineItems = lineItem :: model.lineItems, lineItemAmount = 0, lineItemName = "" } ! []

                Err err ->
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

                        newModel =
                            { model
                                | lineItems = List.map map model.lineItems
                                , lineItem = Just newLineItem
                            }
                    in
                        withSummaries newModel ! []

                Err err ->
                    model ! []

        SaveLineItem ->
            if (String.isEmpty model.lineItemName) then
                model ! []
            else
                case model.lineItem of
                    Just lineItem ->
                        ( model, updateLineItemCmd { lineItem | name = model.lineItemName, amount = model.lineItemAmount } )

                    Nothing ->
                        ( model, addLineItem <| lineItemEncoder <| newLineItem model.lineItemName model.lineItemAmount )

        DeleteLineItem lineItem ->
            ( model, deleteLineItem <| lineItemEncoder lineItem )

        LineItemDeleted value ->
            case JD.decodeValue lineItemDecoder value of
                Ok lineItem ->
                    withSummaries { model | lineItems = deleteLineItemFromList lineItem model.lineItems } ! []

                Err err ->
                    model ! []

        SelectTab num ->
            { model | selectedTab = num } ! []

        ChangeMemberPane memberPane ->
            case memberPane of
                MemberPaneShowDetails member ->
                    { model | memberPane = memberPane, member = Just member, memberName = member.name } ! []

                _ ->
                    { model | memberPane = memberPane, member = Nothing, memberName = "" } ! []

        FilterMembers memberFilter ->
            { model | memberFilter = memberFilter } ! []


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
                |> List.map memberPayment
                |> List.sum

        memberDebitTotal =
            model.members
                |> List.map memberDebit
                |> List.sum

        lineItemTotal =
            sumAmount .lineItems model
    in
        { model
            | totalBalance = memberPaymentsTotal + lineItemTotal
            , totalMemberDebit = memberPaymentsTotal - memberDebitTotal
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ memberAdded MemberAdded
        , memberUpdated MemberUpdated
        , lineItemAdded LineItemAdded
        , lineItemUpdated LineItemUpdated
        , lineItemDeleted LineItemDeleted
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
