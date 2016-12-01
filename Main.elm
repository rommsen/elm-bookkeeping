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
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Color as Color
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Typography as Typo
import Material.Grid as Grid


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


type alias Mdl =
    Material.Model


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
    , mdl : Material.Model
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
    , mdl = Material.model
    }


init : ( Model, Cmd Msg )
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
    | DeleteLineItem LineItem
    | SelectTab Int
    | ChangeMemberPane MemberPane
    | Mdl (Material.Msg Msg)


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
                        ( model, addMemberPort <| memberEncoder <| memberWithName model.memberName )

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
                    withSummaries { model | lineItems = lineItem :: model.lineItems } ! []

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
                        ( model, addLineItemPort <| lineItemEncoder <| newLineItem model.lineItemName model.lineItemAmount )

        DeleteLineItem lineItem ->
            { model
                | lineItems = deleteLineItem lineItem model.lineItems
                , totalBalance = model.totalBalance - lineItem.amount
            }
                ! []

        SelectTab num ->
            { model | selectedTab = num } ! []

        ChangeMemberPane memberPane ->
            case memberPane of
                MemberPaneShowDetails member ->
                    { model | memberPane = memberPane, member = Just member, memberName = member.name } ! []

                _ ->
                    { model | memberPane = memberPane, member = Nothing, memberName = "" } ! []

        Mdl msg_ ->
            Material.update msg_ model


updateMemberCmd : Member -> Cmd msg
updateMemberCmd =
    memberEncoder >> updateMemberPort


updateLineItemCmd : LineItem -> Cmd msg
updateLineItemCmd =
    lineItemEncoder >> updateMemberPort


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteMonthFromList : Month -> List Month -> List Month
deleteMonthFromList month list =
    List.filter (not << (monthEquals month)) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    ( a.month, a.year ) == ( b.month, b.year )


deleteLineItem : LineItem -> List LineItem -> List LineItem
deleteLineItem lineItem list =
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
        ]


port addMemberPort : JD.Value -> Cmd msg


port updateMemberPort : JD.Value -> Cmd msg


port memberAdded : (JD.Value -> msg) -> Sub msg


port memberUpdated : (JD.Value -> msg) -> Sub msg


port addLineItemPort : JD.Value -> Cmd msg


port updateLineItemPort : JD.Value -> Cmd msg


port lineItemAdded : (JD.Value -> msg) -> Sub msg


port lineItemUpdated : (JD.Value -> msg) -> Sub msg



-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedTabs
            , Layout.onSelectTab SelectTab
            , Layout.selectedTab model.selectedTab
            ]
            { header = viewHeader model
            , drawer = []
            , tabs = ( [ text "Members", text "LineItems" ], [ Color.background (Color.color Color.Teal Color.S400) ] )
            , main = [ viewBody model ]
            }


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ Layout.row
        []
        [ Layout.title [] [ text "Bookkeeping" ]
        , Layout.spacer
        , Layout.title []
            [ span
                []
                [ text <| "Total: " ++ toString model.totalBalance ]
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


membersView : Model -> Html Msg
membersView model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.Desktop 2, Grid.size Grid.Tablet 1, Grid.size Grid.Phone 0 ] []
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ memberActionsView model
            , memberListView model
            ]
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ memberPaneView model
            ]
        , Grid.cell [ Grid.size Grid.Desktop 2, Grid.size Grid.Tablet 1, Grid.size Grid.Phone 0 ] []
        , Grid.cell [ Grid.size Grid.Desktop 2, Grid.size Grid.Tablet 1, Grid.size Grid.Phone 0 ] []
        , Grid.cell
            [ Grid.size Grid.All 8 ]
            [ hr [] []
            , text (toString model)
            ]
        , Grid.cell [ Grid.size Grid.Desktop 2, Grid.size Grid.Tablet 1, Grid.size Grid.Phone 0 ] []
        ]


lineItemsView : Model -> Html Msg
lineItemsView model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 2 ] []
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ lineItemListView model ]
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ div []
                [ Options.styled p
                    [ Typo.display1 ]
                    [ text "Line Item" ]
                , lineItemForm model
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 2 ] []
        , Grid.cell [ Grid.size Grid.All 2 ] []
        , Grid.cell
            [ Grid.size Grid.All 8 ]
            [ hr [] []
            , text (toString model)
            ]
        , Grid.cell [ Grid.size Grid.All 2 ] []
        ]


memberActionsView : Model -> Html Msg
memberActionsView model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 4 ]
            [ Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.ripple
                , Button.onClick <| ChangeMemberPane MemberPaneAddMember
                ]
                [ text "Add Member" ]
            ]
        , Grid.cell [ Grid.size Grid.All 4 ]
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.ripple
                , Button.onClick <| ChangeMemberPane MemberPaneAddMonth
                ]
                [ text "Add Month" ]
            ]
        , Grid.cell [ Grid.size Grid.All 4 ] []
        ]


memberListView : Model -> Html Msg
memberListView model =
    Table.table []
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Balance" ]
                , Table.th [] [ text "" ]
                ]
            ]
        , Table.tbody [] <| List.map (memberItemView model) model.members
        ]


memberItemView : Model -> Member -> Html Msg
memberItemView model member =
    let
        activeIcon =
            if member.active then
                "done"
            else
                "not_interested"
    in
        Table.tr []
            [ Table.td [] [ text member.name ]
            , Table.td [ Table.numeric ] [ text <| toString (memberBalance member) ++ " €" ]
            , Table.td []
                [ Button.render Mdl
                    [ 3 ]
                    model.mdl
                    [ Button.minifab
                    , Button.colored
                    , Button.ripple
                    , Button.onClick <| ChangeMemberPane <| MemberPaneShowDetails member
                    ]
                    [ Icon.i "open_in_new" ]
                , Button.render Mdl
                    [ 4 ]
                    model.mdl
                    [ Button.minifab
                    , Button.colored
                    , Button.ripple
                    , Button.onClick <| ToggleMemberIsActive member
                    ]
                    [ Icon.i activeIcon ]
                ]
            ]


memberPaneView : Model -> Html Msg
memberPaneView model =
    div []
        (case model.memberPane of
            MemberPaneShowNone ->
                []

            MemberPaneShowDetails _ ->
                case model.member of
                    Just member ->
                        [ memberDetailsHeaderView model "Member"
                        , memberForm model
                        , paymentForm model
                        , memberPaymentListView member
                        , memberMonthListView member
                        ]

                    Nothing ->
                        []

            MemberPaneAddMonth ->
                [ memberDetailsHeaderView model "Add month"
                , monthForm model
                ]

            MemberPaneAddMember ->
                [ memberDetailsHeaderView model "Add member"
                , memberForm model
                ]
        )


memberDetailsHeaderView : Model -> String -> Html Msg
memberDetailsHeaderView model header =
    Options.styled p
        [ Typo.display1 ]
        [ text header
        , Button.render
            Mdl
            [ 5 ]
            model.mdl
            [ Button.colored
            , Button.onClick <| ChangeMemberPane MemberPaneShowNone
            ]
            [ text "Cancel" ]
        ]


memberPaymentListView : Member -> Html Msg
memberPaymentListView member =
    div []
        [ Options.styled p
            [ Typo.title ]
            [ text "Payments" ]
        , Table.table []
            [ Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "Date" ]
                    , Table.th [] [ text "Amount" ]
                    ]
                ]
            , Table.tbody [] <| List.map (memberPaymentItemView member) member.payments
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
        Table.tr []
            [ Table.td [] [ text <| toString day ++ "." ++ toString month ++ "." ++ toString year ]
            , Table.td [ Table.numeric ] [ text <| toString payment.amount ++ " €" ]
            ]


memberMonthListView : Member -> Html Msg
memberMonthListView member =
    div []
        [ Options.styled p
            [ Typo.title ]
            [ text "Active in month" ]
        , Table.table []
            [ Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "Name" ]
                    , Table.th [] [ text "Amount" ]
                    , Table.th [] [ text "" ]
                    ]
                ]
            , Table.tbody [] <| List.map (memberMonthItemView member) member.months
            ]
        ]


memberMonthItemView : Member -> Month -> Html Msg
memberMonthItemView member month =
    Table.tr []
        [ Table.td [] [ text <| toString month.month ++ " " ++ toString month.year ]
        , Table.td [ Table.numeric ] [ text <| toString month.amount ++ " €" ]
        , Table.td []
            [ button
                [ onClick <| DeleteMonthFromMember month member ]
                [ text "Delete" ]
            ]
        ]


paymentForm : Model -> Html Msg
paymentForm model =
    Html.form [ onSubmit CreateMemberPayment ]
        [ Textfield.render Mdl
            [ 6 ]
            model.mdl
            [ Textfield.label "Payment"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputMemberPaymentAmount
            , Textfield.value <| toString model.memberPayment
            ]
        , Button.render
            Mdl
            [ 7 ]
            model.mdl
            [ Button.colored
            , Button.type_ "submit"
            ]
            [ text "Add payment" ]
        ]


memberForm : Model -> Html Msg
memberForm model =
    Html.form [ onSubmit SaveMemberName ]
        [ Textfield.render Mdl
            [ 8 ]
            model.mdl
            [ Textfield.label "Name"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputMemberName
            , Textfield.value model.memberName
            ]
        , Button.render
            Mdl
            [ 9 ]
            model.mdl
            [ Button.colored
            , Button.type_ "submit"
            ]
            [ text "Save" ]
        ]


monthOption : a -> Html msg
monthOption month =
    option [ value (toString month) ] [ text (toString month) ]


monthForm : Model -> Html Msg
monthForm model =
    Html.form [ onSubmit AddMonthToActiveMembers ]
        [ select [ on "change" (JD.map SelectMonth monthSelectDecoder) ]
            (List.map monthOption months)
        , br [] []
        , Textfield.render Mdl
            [ 10 ]
            model.mdl
            [ Textfield.label "Year"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputMonthYear
            , Textfield.value <| toString model.month.year
            ]
        , br [] []
        , Textfield.render Mdl
            [ 11 ]
            model.mdl
            [ Textfield.label "Amount"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputMonthAmount
            , Textfield.value <| toString model.month.amount
            ]
        , br [] []
        , Button.render
            Mdl
            [ 11 ]
            model.mdl
            [ Button.colored
            , Button.ripple
            , Button.type_ "submit"
            ]
            [ text "Add month to active members" ]
        ]


lineItemListView : Model -> Html Msg
lineItemListView model =
    Table.table []
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Amount" ]
                , Table.th [] [ text "" ]
                ]
            ]
        , Table.tbody [] <| List.map (lineItemView model) model.lineItems
        ]


lineItemView : Model -> LineItem -> Html Msg
lineItemView model lineItem =
    Table.tr []
        [ Table.td [] [ text <| lineItem.name ]
        , Table.td [ Table.numeric ] [ text <| toString lineItem.amount ++ " €" ]
        , Table.td []
            [ Button.render Mdl
                [ 13 ]
                model.mdl
                [ Button.minifab
                , Button.colored
                , Button.ripple
                , Button.onClick <| SelectLineItem lineItem
                ]
                [ Icon.i "open_in_new" ]
            , Button.render Mdl
                [ 14 ]
                model.mdl
                [ Button.minifab
                , Button.colored
                , Button.ripple
                , Button.onClick <| DeleteLineItem lineItem
                ]
                [ Icon.i "delete" ]
            ]
        ]


lineItemForm : Model -> Html Msg
lineItemForm model =
    Html.form [ onSubmit SaveLineItem ]
        [ Textfield.render Mdl
            [ 15 ]
            model.mdl
            [ Textfield.label "Name"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputLineItemName
            , Textfield.value model.lineItemName
            ]
        , br [] []
        , Textfield.render Mdl
            [ 16 ]
            model.mdl
            [ Textfield.label "Amount"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputLineItemAmount
            , Textfield.value <| toString model.lineItemAmount
            ]
        , br [] []
        , Button.render
            Mdl
            [ 17 ]
            model.mdl
            [ Button.colored
            , Button.ripple
            , Button.type_ "submit"
            ]
            [ text "Save line item" ]
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
