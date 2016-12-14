module Members.View exposing (view)

import Members.Types exposing (..)
import Members.Rest exposing (..)
import Types exposing (Model)
import FormElements exposing (wrapFormElement)
import FormValidation exposing (getFormError)
import Sum exposing (memberBalance)
import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD


view : Model -> Html Msg
view model =
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
                        , paymentForm model.paymentForm
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


paymentForm : PaymentForm -> Html Msg
paymentForm form =
    let
        amountError =
            getFormError "amount" form.errors

        amountInput =
            wrapFormElement "Payment" amountError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", amountError /= Nothing )
                        ]
                    , onInput InputMemberPaymentAmount
                    , placeholder "Payment"
                    , value form.amount
                    ]
                    []

        submitButton =
            p [ class "control" ]
                [ button
                    [ type_ "submit"
                    , class "button is-primary"
                    ]
                    [ text "Make Payment"
                    ]
                ]
    in
        Html.form [ onSubmit CreateMemberPayment ]
            [ amountInput
            , submitButton
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


monthOption : a -> Html Msg
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
