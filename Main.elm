module Main exposing (..)

import List
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
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
    { id : Int
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


initialModel : Model
initialModel =
    { members =
        [ { id = 1, name = "Roman Sachse", active = True, months = [], payments = [], balance = 0 }
        , { id = 2, name = "Lena Sachse", active = False, months = [], payments = [], balance = 0 }
        ]
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


monthDecoder : Json.Decoder Date.Month
monthDecoder =
    targetValue
        |> Json.andThen
            (\string ->
                case string of
                    "Jan" ->
                        Json.succeed Date.Jan

                    "Feb" ->
                        Json.succeed Date.Feb

                    "Mar" ->
                        Json.succeed Date.Mar

                    "Apr" ->
                        Json.succeed Date.Apr

                    "May" ->
                        Json.succeed Date.May

                    "Jun" ->
                        Json.succeed Date.Jun

                    "Jul" ->
                        Json.succeed Date.Jul

                    "Aug" ->
                        Json.succeed Date.Aug

                    "Sep" ->
                        Json.succeed Date.Sep

                    "Oct" ->
                        Json.succeed Date.Oct

                    "Nov" ->
                        Json.succeed Date.Nov

                    "Dec" ->
                        Json.succeed Date.Dec

                    _ ->
                        Json.fail "month not available"
            )



-- UPDATE


type Msg
    = SaveMember
    | CancelMember
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | SaveMemberPayment
    | SelectMonth Date.Month
    | InputMonthYear String
    | InputMonthAmount String
    | AddMonth
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

        SaveMember ->
            if (String.isEmpty model.memberName) then
                model ! []
            else
                saveMember model ! []

        ToggleMemberIsActive member ->
            let
                map member_ =
                    if member_.id == member.id then
                        { member_ | active = not <| member_.active }
                    else
                        member_
            in
                { model | members = List.map map model.members } ! []

        SaveMemberPayment ->
            case model.member of
                Just member ->
                    addMemberPayment model member ! []

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

        AddMonth ->
            { model | members = List.map (addMonthToActiveMembers model.month) model.members } ! []

        DeleteMonthFromMember month member ->
            let
                map member_ =
                    if member_.id == member.id then
                        { member_ | months = deleteMonthFromList month member.months, balance = member.balance + month.amount }
                    else
                        member_
            in
                { model | members = List.map map model.members } ! []

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


addMonthToActiveMembers : Month -> Member -> Member
addMonthToActiveMembers month member =
    if member.active == True && not (memberHasMonth member month) then
        { member | months = month :: member.months, balance = member.balance - month.amount }
    else
        member


memberHasMonth : Member -> Month -> Bool
memberHasMonth member month =
    List.any (monthEquals month) member.months


deleteMonthFromList : Month -> List Month -> List Month
deleteMonthFromList month list =
    List.filter (not << (monthEquals month)) list


monthEquals : Month -> Month -> Bool
monthEquals a b =
    ( a.month, a.year ) == ( b.month, b.year )


saveMember : Model -> Model
saveMember model =
    case model.member of
        Just member ->
            editMember model member

        Nothing ->
            addMember model


editMember : Model -> Member -> Model
editMember model member =
    let
        map member_ =
            if member_.id == member.id then
                { member_ | name = model.memberName }
            else
                member_
    in
        { model
            | members = List.map map model.members
            , member = Nothing
            , memberName = ""
        }


addMember : Model -> Model
addMember model =
    let
        newMember =
            { id = List.length model.members
            , name = model.memberName
            , active = True
            , months = []
            , payments = []
            , balance = 0
            }
    in
        { model
            | members = newMember :: model.members
            , memberName = ""
        }


addMemberPayment : Model -> Member -> Model
addMemberPayment model member =
    let
        map member_ =
            if member_.id == member.id then
                { member_
                    | payments = Payment model.memberPayment :: member.payments
                    , balance = member.balance + model.memberPayment
                }
            else
                member_
    in
        { model
            | members = List.map map model.members
            , memberPayment = 0
            , totalBalance = model.totalBalance + model.memberPayment
        }


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
    Sub.none



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
    case model.memberPane of
        MemberPaneShowNone ->
            div [] [ text "" ]

        MemberPaneShowDetails member ->
            div []
                [ memberDetailsHeaderView model "Member"
                , memberForm model
                , paymentForm model
                , memberMonthListView member
                ]

        MemberPaneAddMonth ->
            div []
                [ memberDetailsHeaderView model "Add month"
                , monthForm model
                ]

        MemberPaneAddMember ->
            div []
                [ memberDetailsHeaderView model "Add member"
                , memberForm model
                ]


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
    Html.form [ onSubmit SaveMember ]
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
    Html.form [ onSubmit AddMonth ]
        [ select [ on "change" (Json.map SelectMonth monthDecoder) ]
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
