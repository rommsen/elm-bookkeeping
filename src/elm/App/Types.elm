module App.Types exposing (..)

import Form.Validation exposing (..)


type alias Model =
    { selectedTab : Tab
    , loggedIn : Bool
    , loginForm : LoginForm
    }


type alias User =
    { email : String
    , password : String
    }


type Tab
    = MemberTab
    | LineItemTab


type Msg
    = SelectTab Tab
    | InputEmail String
    | InputPassword String
    | Login
    | Logout
    | Auth Bool


type alias LoginForm =
    { email : String
    , password : String
    , errors : List Error
    }


emptyLoginForm : LoginForm
emptyLoginForm =
    LoginForm "" "" []
