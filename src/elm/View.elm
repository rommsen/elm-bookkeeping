module View exposing (view)

import Rest exposing (..)
import Types exposing (..)
import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewBody model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    section [ class "hero is-info" ]
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
                                [ classList
                                    [ ( "notification", True )
                                    , ( "is-danger", model.totalBalance < 0 )
                                    , ( "is-success", model.totalBalance >= 0 )
                                    ]
                                ]
                                [ p [ class "title is-4" ]
                                    [ text <| "Balance:  " ++ toString model.totalBalance ++ "€" ]
                                ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box" ]
                            [ div
                                [ classList
                                    [ ( "notification", True )
                                    , ( "is-danger", model.totalMemberDebit < 0 )
                                    , ( "is-success", model.totalMemberDebit >= 0 )
                                    ]
                                ]
                                [ p [ class "title is-4" ]
                                    [ text <| "Members:  " ++ toString model.totalMemberDebit ++ "€" ]
                                ]
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
                            [ classList [ ( "is-active", model.selectedTab == MemberTab ) ]
                            , onClick <| SelectTab MemberTab
                            ]
                            [ a []
                                [ span [ class "icon" ] [ i [ class "fa fa-users" ] [] ]
                                , span [] [ text "Members" ]
                                ]
                            ]
                        , li
                            [ classList [ ( "is-active", model.selectedTab == LineItemTab ) ]
                            , onClick <| SelectTab LineItemTab
                            ]
                            [ a []
                                [ span [ class "icon" ] [ i [ class "fa fa-list" ] [] ]
                                , span [] [ text "Line Items" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        MemberTab ->
            membersView model

        LineItemTab ->
            lineItemsView model


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
                        , lineItemForm model.lineItemForm
                        ]
                    ]
                ]
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
                [ span [ class "icon" ]
                    [ i [ class "fa fa-plus-circle" ] []
                    ]
                , span [] [ text "Add member" ]
                ]
            ]
        , p [ class "control" ]
            [ button
                [ class "button is-primary"
                , onClick <| ChangeMemberPane MemberPaneAddMonth
                ]
                [ span [ class "icon" ]
                    [ i [ class "fa fa-plus-circle" ] [] ]
                , span [] [ text "Add month" ]
                ]
            ]
        ]


memberListView : Model -> Html Msg
memberListView model =
    let
        members =
            List.filter
                (\member ->
                    case model.memberFilter of
                        MemberFilterAll ->
                            True

                        MemberFilterActive ->
                            member.active

                        MemberFilterInactive ->
                            not member.active
                )
                model.members
    in
        table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [ colspan 3 ]
                        [ div [ class "control has-addons" ]
                            [ button
                                [ class "button is-disabled" ]
                                [ span [ class "icon" ]
                                    [ i [ class "fa fa-filter is-small" ] [] ]
                                , span [] [ text "" ]
                                ]
                            , button
                                [ class "button"
                                , classList [ ( "is-info", model.memberFilter == MemberFilterAll ) ]
                                , onClick <| FilterMembers MemberFilterAll
                                ]
                                [ text "All" ]
                            , button
                                [ class "button"
                                , classList [ ( "is-info", model.memberFilter == MemberFilterActive ) ]
                                , onClick <| FilterMembers MemberFilterActive
                                ]
                                [ text "Active" ]
                            , button
                                [ class "button"
                                , classList [ ( "is-info", model.memberFilter == MemberFilterInactive ) ]
                                , onClick <| FilterMembers MemberFilterInactive
                                ]
                                [ text "Inactive" ]
                            ]
                        ]
                    ]
                , tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Balance" ]
                    , th [] []
                    ]
                ]
            , tbody [] <| List.map (memberItemView model) members
            ]


memberItemView : Model -> Member -> Html Msg
memberItemView model member =
    tr []
        [ td [] [ text member.name ]
        , td [] [ text <| toString (memberBalance member) ++ " €" ]
        , td []
            [ div [ class "control is-grouped" ]
                [ p [ class "control" ]
                    [ button
                        [ classList
                            [ ( "button", True )
                            , ( "is-primary", True )
                            , ( "is-inverted", Just member /= model.member )
                            ]
                        , onClick <| ChangeMemberPane <| MemberPaneShowDetails member
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fa fa-id-card-o" ] [] ]
                        ]
                    ]
                , p [ class "control" ]
                    [ button
                        [ classList
                            [ ( "button", True )
                            , ( "is-inverted", True )
                            , ( "fa", True )
                            , ( "is-primary", member.active )
                            , ( "is-danger", not member.active )
                            ]
                        , onClick <| ToggleMemberIsActive member
                        ]
                        [ span [ class "icon" ]
                            [ i
                                [ classList
                                    [ ( "fa", True )
                                    , ( "fa-check", member.active )
                                    , ( "fa-pause", not member.active )
                                    ]
                                ]
                                []
                            ]
                        ]
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
                        , memberNameForm model.memberNameForm
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
                , monthForm model.monthForm
                ]

        MemberPaneAddMember ->
            div [ class "tile is-child box" ]
                [ memberDetailsHeaderView model "Add member"
                , memberNameForm model.memberNameForm
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
        , table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Date" ]
                    , th [] [ text "Amount" ]
                    , th [] [ text "" ]
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
            , td []
                [ button
                    [ class "button is-danger is-inverted"
                    , onClick <| DeleteMemberPayment member payment
                    ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-trash-o" ] [] ]
                    ]
                ]
            ]


memberMonthListView : Member -> Html Msg
memberMonthListView member =
    div []
        [ h1 [ class "title" ]
            [ text "Active in month" ]
        , table [ class "table" ]
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
                [ span [ class "icon" ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                ]
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


memberNameForm : MemberNameForm -> Html Msg
memberNameForm form =
    let
        nameError =
            getFormError "name" form.errors

        nameInput =
            wrapFormElement "Name" nameError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", nameError /= Nothing )
                        ]
                    , onInput InputMemberName
                    , placeholder "Name"
                    , value form.name
                    ]
                    []

        submitButton =
            p [ class "control" ]
                [ button
                    [ type_ "submit"
                    , class "button is-primary"
                    ]
                    [ text "Save"
                    ]
                ]
    in
        Html.form [ onSubmit SaveMemberName ]
            [ nameInput
            , submitButton
            ]


monthOption : a -> Html msg
monthOption month =
    option [ value <| toString month ] [ text <| toString month ]


monthForm : MonthForm -> Html Msg
monthForm form =
    let
        yearError =
            getFormError "year" form.errors

        amountError =
            getFormError "amount" form.errors

        monthSelect =
            wrapFormElement "Month" Nothing <|
                span [ class "select" ]
                    [ select
                        [ on "change" <| JD.map SelectMonth monthSelectDecoder ]
                        (List.map monthOption months)
                    ]

        yearInput =
            wrapFormElement "Year" yearError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", yearError /= Nothing )
                        ]
                    , onInput InputMonthYear
                    , placeholder "Year"
                    , value form.year
                    ]
                    []

        amountInput =
            wrapFormElement "Amount" amountError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", amountError /= Nothing )
                        ]
                    , onInput InputMonthAmount
                    , placeholder "Amount"
                    , value form.amount
                    ]
                    []

        submitButton =
            p [ class "control" ]
                [ button [ type_ "submit", class "button is-primary" ]
                    [ text "Add month to active members"
                    ]
                ]
    in
        Html.form [ onSubmit AddMonthToActiveMembers ]
            [ monthSelect
            , yearInput
            , amountInput
            , submitButton
            ]


lineItemListView : Model -> Html Msg
lineItemListView model =
    table [ class "table" ]
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
                [ button
                    [ classList
                        [ ( "button", True )
                        , ( "is-primary", True )
                        , ( "is-inverted", Just lineItem /= model.lineItem )
                        ]
                    , onClick <| SelectLineItem lineItem
                    ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-pencil-square-o" ] [] ]
                    ]
                , p [ class "control" ]
                    [ button
                        [ class "button is-danger is-inverted"
                        , onClick <| DeleteLineItem lineItem
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fa fa-trash-o" ] [] ]
                        ]
                    ]
                ]
            ]
        ]


lineItemForm : LineItemForm -> Html Msg
lineItemForm form =
    let
        nameError =
            getFormError "name" form.errors

        amountError =
            getFormError "amount" form.errors

        nameInput =
            wrapFormElement "Name" nameError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", nameError /= Nothing )
                        ]
                    , onInput InputLineItemName
                    , placeholder "Name"
                    , value form.name
                    ]
                    []

        amountInput =
            wrapFormElement "Amount" amountError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", amountError /= Nothing )
                        ]
                    , onInput InputLineItemAmount
                    , placeholder "Amount"
                    , value form.amount
                    ]
                    []

        submitButton =
            button
                [ type_ "submit"
                , class "button is-primary"
                ]
                [ text "Save line item" ]

        {- I need to make this a link otherwise the form gets submitted... -}
        cancelButton =
            a
                [ class "button is-link"
                , onClick CancelLineItem
                ]
                [ text "Cancel" ]
    in
        Html.form [ onSubmit SaveLineItem ]
            [ nameInput
            , amountInput
            , div [ class "control is-grouped" ]
                [ submitButton
                , cancelButton
                ]
            ]


wrapFormElement : String -> Maybe String -> Html Msg -> Html Msg
wrapFormElement elementLabel elementError element =
    let
        withError =
            case elementError of
                Just error ->
                    [ element
                    , i [ class "fa fa-warning" ] []
                    , span [ class "help is-danger" ] [ text error ]
                    ]

                Nothing ->
                    [ element ]
    in
        div [ class "control" ]
            [ label [ class "label" ] [ text elementLabel ]
            , p
                [ classList
                    [ ( "control", True )
                    , ( "has-icon", elementError /= Nothing )
                    , ( "has-icon-right", elementError /= Nothing )
                    ]
                ]
                withError
            ]


getFormError : String -> Dict.Dict String (Maybe String) -> Maybe String
getFormError key errors =
    Maybe.withDefault Nothing (Dict.get key errors)
