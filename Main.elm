port module Main exposing (..)

import List
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline
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
    , balance : Float
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
    { amount : Float }


type alias LineItem =
    { id : Int
    , name : String
    , amount : Float
    }


type alias LineItemInProcess =
    { id : Maybe Int
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
    , lineItem : LineItemInProcess
    , totalBalance : Float
    , memberPane : MemberPane
    , selectedTab : Int
    , mdl : Material.Model
    }


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]


emptyLineItem : LineItemInProcess
emptyLineItem =
    LineItemInProcess Nothing "" 0


memberWithName : String -> Member
memberWithName name =
    Member "" name True [] [] 0


initialModel : Model
initialModel =
    { members = []
    , member = Nothing
    , memberName = ""
    , month = Month Date.Jan 2016 7.5
    , memberPayment = 0
    , lineItems = []
    , lineItem = emptyLineItem
    , totalBalance = 0
    , memberPane = MemberPaneShowNone
    , selectedTab = 0
    , mdl = Material.model
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


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
        |> Json.Decode.Pipeline.optional "payments" (JD.list decodePayment) []
        |> Json.Decode.Pipeline.required "balance" (JD.float)


monthTypeDecoder : JD.Decoder Month
monthTypeDecoder =
    Json.Decode.Pipeline.decode Month
        |> Json.Decode.Pipeline.required "month" (monthStringDecoder)
        |> Json.Decode.Pipeline.required "year" (JD.int)
        |> Json.Decode.Pipeline.required "amount" (JD.float)


decodePayment : JD.Decoder Payment
decodePayment =
    Json.Decode.Pipeline.decode Payment
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
        [ ( "amount", JE.float payment.amount ) ]


memberEncoder : Member -> JE.Value
memberEncoder member =
    JE.object
        [ ( "id", JE.string <| member.id )
        , ( "name", JE.string <| member.name )
        , ( "active", JE.bool <| member.active )
        , ( "months", JE.list <| List.map monthEncoder <| member.months )
        , ( "payments", JE.list <| List.map paymentEncoder <| member.payments )
        , ( "balance", JE.float <| member.balance )
        ]


type Msg
    = SaveMemberName
    | CancelMember
    | MemberAdded JD.Value
    | MemberUpdated JD.Value
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | SaveMemberPayment
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
    | DeleteLineItem LineItem
    | SelectTab Int
    | ChangeMemberPane MemberPane
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMemberName name ->
            { model | memberName = name } ! []

        CancelMember ->
            { model | member = Nothing, memberName = "", memberPayment = 0, memberPane = MemberPaneShowNone } ! []

        SaveMemberName ->
            if (String.isEmpty model.memberName) then
                model ! []
            else
                saveMember model

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
                        _ =
                            Debug.log "MemberUpdated" newMember

                        map member =
                            if member.id == newMember.id then
                                newMember
                            else
                                member
                    in
                        { model | members = List.map map model.members, member = Just newMember, memberName = newMember.name } ! []

                Err err ->
                    model ! []

        ToggleMemberIsActive member ->
            ( model, updateMemberCmd { member | active = not <| member.active } )

        SaveMemberPayment ->
            case model.member of
                Just member ->
                    let
                        newMember =
                            { member
                                | payments = Payment model.memberPayment :: member.payments
                                , balance = member.balance + model.memberPayment
                            }
                    in
                        ( { model | memberPayment = 0 }, updateMemberCmd newMember )

                Nothing ->
                    model ! []

        InputMemberPaymentAmount amount ->
            case ( String.toFloat amount, model ) of
                ( Ok val, { month } ) ->
                    { model | memberPayment = val } ! []

                ( Err bla, _ ) ->
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
                        |> List.map (\m -> { m | months = model.month :: m.months, balance = m.balance - model.month.amount })
                        |> List.map updateMemberCmd
            in
                model ! cmds

        DeleteMonthFromMember month member ->
            let
                newMember =
                    { member
                        | months = deleteMonthFromList month member.months
                        , balance = member.balance + month.amount
                    }
            in
                ( model, updateMemberCmd newMember )

        SelectLineItem { id, name, amount } ->
            { model | lineItem = LineItemInProcess (Just id) name amount } ! []

        CancelLineItem ->
            { model | lineItem = emptyLineItem } ! []

        InputLineItemName name ->
            let
                { lineItem } =
                    model
            in
                { model | lineItem = { lineItem | name = name } } ! []

        InputLineItemAmount amount ->
            case ( String.toFloat amount, model ) of
                ( Ok val, { lineItem } ) ->
                    { model | lineItem = { lineItem | amount = val } } ! []

                ( Err bla, _ ) ->
                    model ! []

        SaveLineItem ->
            if (String.isEmpty model.lineItem.name) then
                model ! []
            else
                saveLineItem model ! []

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


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteMonthFromList : Month -> List Month -> List Month
deleteMonthFromList month list =
    List.filter (not << (monthEquals month)) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    ( a.month, a.year ) == ( b.month, b.year )


saveMember : Model -> ( Model, Cmd Msg )
saveMember model =
    case model.member of
        Just member ->
            ( model, updateMemberCmd { member | name = model.memberName } )

        Nothing ->
            ( model, addMemberPort <| memberEncoder <| memberWithName model.memberName )


saveLineItem : Model -> Model
saveLineItem model =
    case model.lineItem.id of
        Just id ->
            editLineItem model id

        Nothing ->
            addLineItem model


editLineItem : Model -> Int -> Model
editLineItem model id =
    let
        newAmount =
            model.lineItem.amount

        oldAmount =
            List.foldl
                (\lineItem amount ->
                    if lineItem.id == id then
                        lineItem.amount
                    else
                        0
                )
                0
                model.lineItems

        map lineItem =
            if lineItem.id == id then
                { lineItem
                    | name = model.lineItem.name
                    , amount = newAmount
                }
            else
                lineItem
    in
        { model
            | lineItems = List.map map model.lineItems
            , lineItem = emptyLineItem
            , totalBalance = model.totalBalance - oldAmount + newAmount
        }


addLineItem : Model -> Model
addLineItem model =
    let
        newLineItem =
            { id = List.length model.lineItems
            , name = model.lineItem.name
            , amount = model.lineItem.amount
            }
    in
        { model
            | lineItems = newLineItem :: model.lineItems
            , lineItem = emptyLineItem
            , totalBalance = model.totalBalance + newLineItem.amount
        }


deleteLineItem : LineItem -> List LineItem -> List LineItem
deleteLineItem lineItem list =
    List.filter (\{ id } -> id /= lineItem.id) list



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ memberAdded MemberAdded
        , memberUpdated MemberUpdated
        ]


port addMemberPort : JD.Value -> Cmd msg


port updateMemberPort : JD.Value -> Cmd msg


port memberAdded : (JD.Value -> msg) -> Sub msg


port memberUpdated : (JD.Value -> msg) -> Sub msg



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
        [ Grid.cell [ Grid.size Grid.All 2 ] []
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ memberActionsView model
            , memberListView model
            ]
        , Grid.cell
            [ Grid.size Grid.All 4 ]
            [ memberPaneView model
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
            , Table.td [ Table.numeric ] [ text <| toString member.balance ++ " €" ]
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
            , Button.onClick CancelMember
            ]
            [ text "Cancel" ]
        ]


memberMonthListView : Member -> Html Msg
memberMonthListView member =
    Table.table []
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Amount" ]
                , Table.th [] [ text "" ]
                ]
            ]
        , Table.tbody [] <| List.map (memberMonthItemView member) member.months
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
    Html.form [ onSubmit SaveMemberPayment ]
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
            , Textfield.value model.lineItem.name
            ]
        , br [] []
        , Textfield.render Mdl
            [ 16 ]
            model.mdl
            [ Textfield.label "Amount"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.onInput InputLineItemAmount
            , Textfield.value <| toString model.lineItem.amount
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
            [ text "Add line item" ]
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
