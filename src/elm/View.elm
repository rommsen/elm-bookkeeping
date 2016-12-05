module View exposing (view)

import Rest exposing (..)
import Types exposing (..)
import Date
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
                        , lineItemForm model
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
        table [ class "table is-striped " ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Balance" ]
                    , th []
                        [ div [ class "control is-grouped" ]
                            [ p [ class "control" ]
                                [ button
                                    [ class "button"
                                    , classList [ ( "is-active", model.memberFilter == MemberFilterAll ) ]
                                    , onClick <| FilterMembers MemberFilterAll
                                    ]
                                    [ text "All" ]
                                ]
                            , p [ class "control" ]
                                [ button
                                    [ class "button"
                                    , classList [ ( "is-active", model.memberFilter == MemberFilterActive ) ]
                                    , onClick <| FilterMembers MemberFilterActive
                                    ]
                                    [ text "Active" ]
                                ]
                            , p [ class "control" ]
                                [ button
                                    [ class "button"
                                    , classList [ ( "is-active", model.memberFilter == MemberFilterInactive ) ]
                                    , onClick <| FilterMembers MemberFilterInactive
                                    ]
                                    [ text "Inactive" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , tbody [] <| List.map (memberItemView model) members
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
                    [ text "Delete" ]
                ]
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
        , div [ class "control is-grouped" ]
            [ p [ class "control" ]
                [ button
                    [ class "button is-primary"
                    , type_ "submit"
                    ]
                    [ text "Save line item" ]
                ]
            , p [ class "control" ]
                [ button
                    [ class "button is-link"
                    , onClick CancelLineItem
                    ]
                    [ text "Cancel" ]
                ]
            ]
        ]
