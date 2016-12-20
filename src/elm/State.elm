port module State exposing (init, subscriptions, update)

import Types exposing (..)
import App.Types exposing (Tab(..))
import App.State
import LineItems.State
import Members.State


init : ( Model, Cmd Msg )
init =
    let
        ( membersInitModel, membersCmd ) =
            Members.State.init

        ( lineItemsInitModel, lineItemsCmd ) =
            LineItems.State.init

        initModel =
            { members = membersInitModel
            , lineItems = lineItemsInitModel
            , totalBalance = 0
            , selectedTab = MemberTab
            }

        cmds =
            Cmd.batch
                [ Cmd.map MemberMsg membersCmd
                , Cmd.map LineItemMsg lineItemsCmd
                ]
    in
        ( initModel, cmds )


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
                ( membersModel, memberCmd ) =
                    Members.State.update memberMsg model.members
            in
                ( withSummaries { model | members = membersModel }
                , Cmd.map MemberMsg memberCmd
                )

        LineItemMsg lineItemMsg ->
            let
                ( lineItemsModel, lineItemsCmd ) =
                    LineItems.State.update lineItemMsg model.lineItems
            in
                ( withSummaries { model | lineItems = lineItemsModel }
                , Cmd.map LineItemMsg lineItemsCmd
                )


withSummaries : Model -> Model
withSummaries model =
    { model | totalBalance = model.members.memberPaymentsTotal + model.lineItems.lineItemTotal }


subscriptions : Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Sub.map MemberMsg Members.State.subscriptions
        , Sub.map LineItemMsg LineItems.State.subscriptions
        ]
