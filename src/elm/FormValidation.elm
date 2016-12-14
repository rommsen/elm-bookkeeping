module FormValidation exposing (..)

import Dict


type alias FormErrors =
    Dict.Dict String (Maybe String)


type alias FormError =
    ( String, Maybe String )


validate : (a -> b) -> a -> b
validate validator record =
    validator record


validateAll : List (a -> b) -> a -> List b
validateAll validators record =
    List.map (\validator -> validator record) validators


validateNotBlank : String -> String -> FormError
validateNotBlank name string =
    case stringNotBlankResult string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This should not be empty" )


stringNotBlankResult : String -> Result String String
stringNotBlankResult string =
    if String.isEmpty string then
        Err "string is empty"
    else
        Ok string


validateFloat : String -> String -> FormError
validateFloat name string =
    case String.toFloat string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This is not a valid number" )


validateInt : String -> String -> FormError
validateInt name string =
    case String.toInt string of
        Ok _ ->
            ( name, Nothing )

        Err _ ->
            ( name, Just "This is not a valid number" )


getFormError : String -> Dict.Dict String (Maybe String) -> Maybe String
getFormError key errors =
    Maybe.withDefault Nothing (Dict.get key errors)
