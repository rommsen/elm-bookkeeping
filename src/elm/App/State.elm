port module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)
import Form.Validation exposing (..)


initialModel : Model
initialModel =
    { selectedTab = MemberTab
    , loggedIn = False
    , loginForm = emptyLoginForm
    }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            { model | selectedTab = tab } ! []

        InputEmail email ->
            let
                form =
                    model.loginForm
            in
                { model | loginForm = { form | email = email } } ! []

        InputPassword password ->
            let
                form =
                    model.loginForm
            in
                { model | loginForm = { form | password = password } } ! []

        Login ->
            let
                form =
                    model.loginForm

                loginForm =
                    { form | errors = validateLoginForm form }

                newModel =
                    { model | loginForm = loginForm }
            in
                if List.isEmpty loginForm.errors then
                    ( newModel, login <| User form.email form.password )
                else
                    newModel ! []

        Logout ->
            ( model, logout "" )

        Auth loggedIn ->
            { model | loggedIn = loggedIn, loginForm = emptyLoginForm } ! []


validateLoginForm : LoginForm -> List Error
validateLoginForm form =
    begin form
        |> validate (validateNotBlank "email" << .email)
        |> validate (validateNotBlank "password" << .password)
        |> extractErrors



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ auth Auth ]


port login : User -> Cmd msg


port logout : String -> Cmd msg


port auth : (Bool -> msg) -> Sub msg
