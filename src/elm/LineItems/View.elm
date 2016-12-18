module LineItems.View exposing (..)

import LineItems.Types exposing (..)
import FormElements exposing (wrapFormElement)
import FormValidation exposing (getFormError)
import Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Model -> Html Msg
view model =
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
