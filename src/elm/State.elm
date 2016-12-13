port module State exposing (init, subscriptions, update)

import Types exposing (..)
import App.Types exposing (..)
import App.State
import LineItems.Types exposing (..)
import LineItems.State
import Members.Types exposing (..)
import Members.State


initialModel : Model
initialModel =
    { members = []
    , member = Nothing
    , memberNameForm = emptyMemberNameForm
    , paymentForm = emptyPaymentForm
    , monthForm = emptyMonthForm
    , lineItems = []
    , lineItem = Nothing
    , lineItemForm = emptyLineItemForm
    , totalBalance = 0
    , totalMemberDebit = 0
    , memberPane = MemberPaneShowNone
    , memberFilter = MemberFilterAll
    , selectedTab = MemberTab
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Types.Msg -> Model -> ( Model, Cmd Types.Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( newModel, cmd ) =
                    App.State.update appMsg model
            in
                ( newModel, Cmd.map AppMsg cmd )

        MemberMsg memberMsg ->
            let
                ( newModel, cmd ) =
                    Members.State.update memberMsg model
            in
                ( newModel, Cmd.map MemberMsg cmd )

        LineItemMsg lineItemMsg ->
            let
                ( newModel, cmd ) =
                    LineItems.State.update lineItemMsg model
            in
                ( newModel, Cmd.map LineItemMsg cmd )


subscriptions : Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Sub.map MemberMsg Members.State.subscriptions
        , Sub.map LineItemMsg LineItems.State.subscriptions
        ]
