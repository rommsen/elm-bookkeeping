port module Main exposing (..)

import List
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline
import Task


-- MODEL


type alias Member =
    { id : String
    , name : String
    , active : Bool
    , months : List Month
    , payments : List Payment
    }


type MemberPane
    = MemberPaneShowNone
    | MemberPaneShowDetails Member
    | MemberPaneAddMonth
    | MemberPaneAddMember


type alias Month =
    { month : Date.Month
    , year : Int
    , amount : Float
    }


type alias Payment =
    { amount : Float
    , added : Date.Date
    }


type alias LineItem =
    { id : String
    , name : String
    , amount : Float
    }


type alias Model =
    { members : List Member
    , member : Maybe Member
    , memberName : String
    , memberPayment : Float
    , month : Month
    , lineItems : List LineItem
    , lineItem : Maybe LineItem
    , lineItemName : String
    , lineItemAmount : Float
    , totalBalance : Float
    , totalMemberDebit : Float
    , memberPane : MemberPane
    , selectedTab : Int
    }


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]


newLineItem : String -> Float -> LineItem
newLineItem name amount =
    LineItem "" name amount


memberWithName : String -> Member
memberWithName name =
    Member "" name True [] []


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
    , selectedTab = 0
    }


init =
    ( initialModel, Cmd.none )


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



{--
andThen : (a -> Decoder b)                                              -> Decoder a         -> Decoder b
           funktion von dem decodeteten wert zu einem Decoder von Typ b -> den Decoder für a -> einen Decoder von typ b

 Decoder String = Ein Decoder, der weiß wie er einen String Decodiert
-}


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
        , ( "added", JE.string <| toString payment.added )
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


type Msg
    = SaveMemberName
    | MemberAdded JD.Value
    | MemberUpdated JD.Value
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | CreateMemberPayment
    | SaveMemberPayment Member Payment
    | SelectMonth Date.Month
    | InputMonthYear String
    | InputMonthAmount String
    | AddMonthToActiveMembers
    | DeleteMonthFromMember Month Member
    | SelectLineItem LineItem
    | CancelLineItem
    | InputLineItemName String
    | InputLineItemAmount String
    | SaveLineItem
    | LineItemAdded JD.Value
    | LineItemUpdated JD.Value
    | LineItemDeleted JD.Value
    | DeleteLineItem LineItem
    | SelectTab Int
    | ChangeMemberPane MemberPane


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
            ( model, updateMemberCmd { member | months = deleteMonthFromList month member.months } )

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


updateMemberCmd : Member -> Cmd msg
updateMemberCmd =
    memberEncoder >> updateMember


updateLineItemCmd : LineItem -> Cmd msg
updateLineItemCmd =
    lineItemEncoder >> updateLineItem


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteMonthFromList : Month -> List Month -> List Month
deleteMonthFromList month list =
    List.filter (not << (monthEquals month)) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    ( a.month, a.year ) == ( b.month, b.year )


deleteLineItemFromList : LineItem -> List LineItem -> List LineItem
deleteLineItemFromList lineItem list =
    List.filter (\{ id } -> id /= lineItem.id) list



-- this function can sum any List of records that have an amount property


sumAmount : (a -> List { b | amount : Float }) -> a -> Float
sumAmount property record =
    List.foldl (\payment amount -> amount + payment.amount) 0 (property record)


memberPayment : Member -> Float
memberPayment member =
    sumAmount .payments member


memberDebit : Member -> Float
memberDebit member =
    sumAmount .months member


memberBalance : Member -> Float
memberBalance member =
    memberPayment member - memberDebit member


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewBody model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    section [ class "hero  is-primary" ]
        [ div [ class "hero-head" ]
            [ div [ class "container" ]
                [ div [ class "nav" ]
                    [ div [ class "nav-left" ]
                        [ span [ class "nav-item is-brand" ] [ text "Bookkeeping" ] ]
                    ]
                ]
            ]
        , div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column" ]
                        [ div [ class "box" ]
                            [ div
                                [ class "notification"
                                , classList
                                    [ ( "is-danger", model.totalBalance < 0 )
                                    , ( "is-primary", model.totalBalance >= 0 )
                                    ]
                                ]
                                [ p [ class "title" ] [ text <| "Balance:  " ++ toString model.totalBalance ++ "€" ] ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box" ]
                            [ div
                                [ class "notification"
                                , classList
                                    [ ( "is-danger", model.totalMemberDebit < 0 )
                                    , ( "is-primary", model.totalMemberDebit >= 0 )
                                    ]
                                ]
                                [ p [ class "title" ] [ text <| "Member:  " ++ toString model.totalMemberDebit ++ "€" ] ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "hero-foot" ]
            [ div [ class "container" ]
                [ nav [ class "tabs is-boxed is-medium" ]
                    [ ul []
                        [ li
                            [ classList [ ( "is-active", model.selectedTab == 0 ) ]
                            , onClick <| SelectTab 0
                            ]
                            [ a [] [ text "Members" ]
                            ]
                        , li
                            [ classList [ ( "is-active", model.selectedTab == 1 ) ]
                            , onClick <| SelectTab 1
                            ]
                            [ a [] [ text "LineItems" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            membersView model

        1 ->
            lineItemsView model

        _ ->
            text "404"


modelView : Model -> Html Msg
modelView model =
    div [ class "tile is-ancestor" ]
        [ div [ class "tile is-parent" ]
            [ div [ class "tile is-child box" ]
                [ span [] [ text (toString model) ] ]
            ]
        ]


membersView : Model -> Html Msg
membersView model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "tile is-ancestor" ]
                [ div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ memberActionsView model
                        , memberListView model
                        ]
                    ]
                , div [ class "tile is-parent" ]
                    [ memberPaneView model
                    ]
                ]
            , modelView model
            ]
        ]


lineItemsView : Model -> Html Msg
lineItemsView model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "tile is-ancestor" ]
                [ div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ lineItemListView model ]
                    ]
                , div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ h1 [ class "title" ] [ text "Line Item" ]
                        , lineItemForm model
                        ]
                    ]
                ]
            , modelView model
            ]
        ]


memberActionsView : Model -> Html Msg
memberActionsView model =
    div [ class "control is-grouped" ]
        [ p [ class "control" ]
            [ button
                [ class "button is-primary"
                , onClick <| ChangeMemberPane MemberPaneAddMember
                ]
                [ text "Add Member" ]
            ]
        , p [ class "control" ]
            [ button
                [ class "button is-primary"
                , onClick <| ChangeMemberPane MemberPaneAddMonth
                ]
                [ text "Add Month" ]
            ]
        ]


memberListView : Model -> Html Msg
memberListView model =
    table [ class "table is-striped " ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Balance" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody [] <| List.map (memberItemView model) model.members
        ]


memberItemView : Model -> Member -> Html Msg
memberItemView model member =
    let
        active =
            if member.active then
                "Deaktivieren"
            else
                "Aktivieren"
    in
        tr []
            [ td [] [ text member.name ]
            , td [] [ text <| toString (memberBalance member) ++ " €" ]
            , td []
                [ div [ class "control is-grouped" ]
                    [ p [ class "control" ]
                        [ button
                            [ class "button is-primary is-inverted"
                            , onClick <| ChangeMemberPane <| MemberPaneShowDetails member
                            ]
                            [ text "Details" ]
                        ]
                    , p [ class "control" ]
                        [ button
                            [ class "button is-primary is-inverted"
                            , onClick <| ToggleMemberIsActive member
                            ]
                            [ text active ]
                        ]
                    ]
                ]
            ]


memberPaneView : Model -> Html Msg
memberPaneView model =
    case model.memberPane of
        MemberPaneShowNone ->
            div [] []

        MemberPaneShowDetails _ ->
            case model.member of
                Just member ->
                    div [ class "tile is-child box" ]
                        [ memberDetailsHeaderView model "Member"
                        , memberForm model
                        , hr [] []
                        , paymentForm model
                        , hr [] []
                        , memberPaymentListView member
                        , hr [] []
                        , memberMonthListView member
                        ]

                Nothing ->
                    div [] []

        MemberPaneAddMonth ->
            div [ class "tile is-child box" ]
                [ memberDetailsHeaderView model "Add month"
                , monthForm model
                ]

        MemberPaneAddMember ->
            div [ class "tile is-child box" ]
                [ memberDetailsHeaderView model "Add member"
                , memberForm model
                ]


memberDetailsHeaderView : Model -> String -> Html Msg
memberDetailsHeaderView model header =
    h1 [ class "title" ]
        [ text header
        , button
            [ class "button is-small is-link"
            , onClick <| ChangeMemberPane MemberPaneShowNone
            ]
            [ text "Cancel" ]
        ]


memberPaymentListView : Member -> Html Msg
memberPaymentListView member =
    div []
        [ h1 [ class "title" ] [ text "Payments" ]
        , table [ class "table is-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Date" ]
                    , th [] [ text "Amount" ]
                    ]
                ]
            , tbody [] <| List.map (memberPaymentItemView member) member.payments
            ]
        ]


memberPaymentItemView : Member -> Payment -> Html Msg
memberPaymentItemView member payment =
    let
        day =
            Date.day payment.added

        month =
            Date.month payment.added

        year =
            Date.year payment.added
    in
        tr []
            [ td [] [ text <| toString day ++ "." ++ toString month ++ "." ++ toString year ]
            , td [] [ text <| toString payment.amount ++ " €" ]
            ]


memberMonthListView : Member -> Html Msg
memberMonthListView member =
    div []
        [ h1 [ class "title" ]
            [ text "Active in month" ]
        , table [ class "table is-striped " ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Amount" ]
                    , th [] [ text "" ]
                    ]
                ]
            , tbody [] <| List.map (memberMonthItemView member) member.months
            ]
        ]


memberMonthItemView : Member -> Month -> Html Msg
memberMonthItemView member month =
    tr []
        [ td [] [ text <| toString month.month ++ " " ++ toString month.year ]
        , td [] [ text <| toString month.amount ++ " €" ]
        , td []
            [ button
                [ class "button is-danger is-inverted"
                , onClick <| DeleteMonthFromMember month member
                ]
                [ text "Delete" ]
            ]
        ]


paymentForm : Model -> Html Msg
paymentForm model =
    Html.form [ onSubmit CreateMemberPayment ]
        [ div [ class "control is-grouped" ]
            [ p [ class "control" ]
                [ input
                    [ type_ "text"
                    , class "input"
                    , onInput InputMemberPaymentAmount
                    , placeholder "Payment"
                    , value <| toString model.memberPayment
                    ]
                    []
                ]
            , p [ class "control" ]
                [ button [ type_ "submit", class "button is-primary" ]
                    [ text "AddPayment"
                    ]
                ]
            ]
        ]


memberForm : Model -> Html Msg
memberForm model =
    Html.form [ onSubmit SaveMemberName ]
        [ div [ class "control is-grouped" ]
            [ p
                [ class "control" ]
                [ input
                    [ type_ "text"
                    , class "input"
                    , onInput InputMemberName
                    , placeholder "Name"
                    , value <| model.memberName
                    ]
                    []
                ]
            , p [ class "control" ]
                [ button [ type_ "submit", class "button is-primary" ]
                    [ text "Save"
                    ]
                ]
            ]
        ]


monthOption : a -> Html msg
monthOption month =
    option [ value (toString month) ] [ text (toString month) ]


monthForm : Model -> Html Msg
monthForm model =
    Html.form [ onSubmit AddMonthToActiveMembers ]
        [ label [ class "label" ] [ text "Month" ]
        , p [ class "control" ]
            [ span [ class "select" ]
                [ select
                    [ on "change" (JD.map SelectMonth monthSelectDecoder) ]
                    (List.map monthOption months)
                ]
            ]
        , label [ class "label" ] [ text "Year" ]
        , p [ class "control" ]
            [ input
                [ type_ "text"
                , class "input"
                , onInput InputMonthYear
                , placeholder "Year"
                , value <| toString model.month.year
                ]
                []
            ]
        , label [ class "label" ] [ text "Amount" ]
        , p [ class "control" ]
            [ input
                [ type_ "text"
                , class "input"
                , onInput InputMonthAmount
                , placeholder "Amount"
                , value <| toString model.month.amount
                ]
                []
            ]
        , p [ class "control" ]
            [ button [ type_ "submit", class "button is-primary" ]
                [ text "Add month to active members"
                ]
            ]
        ]


lineItemListView : Model -> Html Msg
lineItemListView model =
    table [ class "table is-striped " ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Amount" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody [] <| List.map (lineItemView model) model.lineItems
        ]


lineItemView : Model -> LineItem -> Html Msg
lineItemView model lineItem =
    tr []
        [ td [] [ text <| lineItem.name ]
        , td [] [ text <| toString lineItem.amount ++ " €" ]
        , td []
            [ div [ class "control is-grouped" ]
                [ p [ class "control" ]
                    [ button
                        [ class "button is-primary is-inverted"
                        , onClick <| SelectLineItem lineItem
                        ]
                        [ text "Change" ]
                    ]
                , p [ class "control" ]
                    [ button
                        [ class "button is-danger is-inverted"
                        , onClick <| DeleteLineItem lineItem
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]


lineItemForm : Model -> Html Msg
lineItemForm model =
    Html.form [ onSubmit SaveLineItem ]
        [ label [ class "label" ] [ text "Name" ]
        , p [ class "control" ]
            [ input
                [ type_ "text"
                , class "input"
                , onInput InputLineItemName
                , placeholder "Name"
                , value <| model.lineItemName
                ]
                []
            ]
        , label [ class "label" ] [ text "Amount" ]
        , p [ class "control" ]
            [ input
                [ type_ "text"
                , class "input"
                , onInput InputLineItemAmount
                , placeholder "Amount"
                , value <| toString model.lineItemAmount
                ]
                []
            ]
        , p [ class "control" ]
            [ button
                [ class "button is-primary"
                , type_ "submit"
                ]
                [ text "Save line item" ]
            ]
        ]



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
