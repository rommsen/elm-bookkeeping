module Main exposing (..)

import List
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


-- MODEL


type alias Member =
    { id : Int
    , name : String
    , active : Bool
    , months : List Month
    , payments : List Payment
    , balance : Float
    }


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
    , amount : String
    }


type alias Model =
    { members : List Member
    , memberId : Maybe Int
    , memberName : String
    , memberPayment : Float
    , month : Month
    , lineItems : List LineItem
    , lineItem : LineItemInProcess
    , totalBalance : Float
    }


months : List Date.Month
months =
    [ Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec ]


emptyLineItem : LineItemInProcess
emptyLineItem =
    LineItemInProcess Nothing "" ""


initialModel : Model
initialModel =
    { members =
        [ { id = 1, name = "Roman Sachse", active = True, months = [], payments = [], balance = 0 }
        , { id = 2, name = "Lena Sachse", active = False, months = [], payments = [], balance = 0 }
        ]
    , memberId = Nothing
    , memberName = ""
    , month = Month Date.Jan 2016 7.5
    , memberPayment = 0
    , lineItems = []
    , lineItem = emptyLineItem
    , totalBalance = 0
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
    = EditMember Member
    | SaveMember
    | CancelMember
    | InputMemberName String
    | ToggleMemberIsActive Member
    | InputMemberPaymentAmount String
    | CancelMemberPayment
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditMember member ->
            { model | memberId = Just member.id, memberName = member.name } ! []

        InputMemberName name ->
            { model | memberName = name } ! []

        CancelMember ->
            { model | memberId = Nothing, memberName = "", memberPayment = 0 } ! []

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
            case model.memberId of
                Just id ->
                    addMemberPayment model id ! []

                Nothing ->
                    model ! []

        CancelMemberPayment ->
            { model | memberId = Nothing, memberName = "", memberPayment = 0 } ! []

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
            { model | lineItem = LineItemInProcess (Just id) name (toString amount) } ! []

        CancelLineItem ->
            { model | lineItem = emptyLineItem } ! []

        InputLineItemName name ->
            let
                { lineItem } =
                    model
            in
                { model | lineItem = { lineItem | name = name } } ! []

        InputLineItemAmount amount ->
            let
                { lineItem } =
                    model
            in
                { model | lineItem = { lineItem | amount = amount } } ! []

        SaveLineItem ->
            if (String.isEmpty model.lineItem.name) || (String.isEmpty model.lineItem.amount) then
                model ! []
            else
                saveLineItem model ! []

        DeleteLineItem lineItem ->
            { model
                | lineItems = deleteLineItem lineItem model.lineItems
                , totalBalance = model.totalBalance - lineItem.amount
            }
                ! []


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
    case model.memberId of
        Just id ->
            editMember model id

        Nothing ->
            addMember model


editMember : Model -> Int -> Model
editMember model id =
    let
        map member =
            if member.id == id then
                { member | name = model.memberName }
            else
                member

        newMembers =
            List.map map model.members
    in
        { model
            | members = newMembers
            , memberId = Nothing
            , memberName = ""
            , memberPayment = 0
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


addMemberPayment : Model -> Int -> Model
addMemberPayment model id =
    let
        map member =
            if member.id == id then
                { member
                    | payments = Payment model.memberPayment :: member.payments
                    , balance = member.balance + model.memberPayment
                }
            else
                member

        newMembers =
            List.map map model.members
    in
        { model
            | members = newMembers
            , memberId = Nothing
            , memberName = ""
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
            Result.withDefault 0 (String.toFloat model.lineItem.amount)

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

        newLineItems =
            List.map map model.lineItems
    in
        { model
            | lineItems = newLineItems
            , lineItem = emptyLineItem
            , totalBalance = model.totalBalance - oldAmount + newAmount
        }


addLineItem : Model -> Model
addLineItem model =
    let
        newLineItem =
            { id = List.length model.lineItems
            , name = model.lineItem.name
            , amount = Result.withDefault 0 (String.toFloat model.lineItem.amount)
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


memberListView : List Member -> Html Msg
memberListView members =
    List.map memberItemView members
        |> ul []


memberItemView : Member -> Html Msg
memberItemView member =
    let
        btnText =
            if member.active then
                "Deactivate"
            else
                "Activate"
    in
        li []
            [ span
                []
                [ text <| member.name ++ " | Balance: " ++ toString member.balance ++ " | " ]
            , button
                [ onClick <| EditMember member ]
                [ text "Select" ]
            , button
                [ onClick <| ToggleMemberIsActive member ]
                [ text btnText ]
            , monthListView member
            ]


memberForm : Model -> Html Msg
memberForm model =
    Html.form [ onSubmit SaveMember ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Member..."
            , onInput InputMemberName
            , value model.memberName
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick CancelMember ] [ text "Cancel" ]
        ]


paymentForm : Model -> Html Msg
paymentForm model =
    Html.form [ onSubmit SaveMemberPayment ]
        [ input
            [ type_ "text"
            , placeholder "Payment..."
            , onInput InputMemberPaymentAmount
            , value <| toString model.memberPayment
            ]
            []
        , button [ type_ "submit" ] [ text "Add payment" ]
        , button [ type_ "button", onClick CancelMemberPayment ] [ text "Cancel" ]
        ]


monthListView : Member -> Html Msg
monthListView member =
    List.map (monthItemView member) member.months
        |> ul []


monthItemView : Member -> Month -> Html Msg
monthItemView member month =
    li []
        [ span
            []
            [ text <| toString month.month ++ " " ++ toString month.year ++ " (" ++ toString month.amount ++ ")" ]
        , button
            [ onClick <| DeleteMonthFromMember month member ]
            [ text "Delete" ]
        ]


monthOption : a -> Html msg
monthOption month =
    option [ value (toString month) ] [ text (toString month) ]


monthForm : Month -> Html Msg
monthForm month =
    Html.form [ onSubmit AddMonth ]
        [ select [ on "change" (Json.map SelectMonth monthDecoder) ] (List.map monthOption months)
        , input
            [ type_ "text"
            , placeholder "Year"
            , onInput InputMonthYear
            , value <| toString month.year
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Amount"
            , onInput InputMonthAmount
            , value <| toString month.amount
            ]
            []
        , button [ type_ "submit" ] [ text "Add month to active members" ]
        ]


lineItemListView : List LineItem -> Html Msg
lineItemListView members =
    List.map lineItemItemView members
        |> ul []


lineItemItemView : LineItem -> Html Msg
lineItemItemView lineItem =
    li []
        [ span
            []
            [ text <| lineItem.name ++ " | Amount: " ++ toString lineItem.amount ++ " | " ]
        , button
            [ onClick <| SelectLineItem lineItem ]
            [ text "Select" ]
        , button
            [ onClick <| DeleteLineItem lineItem ]
            [ text "Delete" ]
        ]


lineItemForm : Model -> Html Msg
lineItemForm model =
    Html.form [ onSubmit SaveLineItem ]
        [ input
            [ type_ "text"
            , placeholder "Name..."
            , onInput InputLineItemName
            , value model.lineItem.name
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Amount..."
            , onInput InputLineItemAmount
            , value model.lineItem.amount
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick CancelLineItem ] [ text "Cancel" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text <| "Total: " ++ toString model.totalBalance ]
        , hr [] []
        , h2 [] [ text "Members" ]
        , memberListView model.members
        , memberForm model
        , paymentForm model
        , hr [] []
        , h2 [] [ text "Month" ]
        , monthForm model.month
        , hr [] []
        , h2 [] [ text "LineItems" ]
        , lineItemListView model.lineItems
        , lineItemForm model
        , hr [] []
        , text (toString model)
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
