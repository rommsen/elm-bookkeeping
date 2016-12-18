module Form.Validation exposing (..)

import List


type Error
    = Error String String


type Validator a
    = Validator (List Error) a


type Status
    = Valid
    | Invalid Error


validate : (a -> Status) -> Validator a -> Validator a
validate f (Validator errors value) =
    case f value of
        Valid ->
            Validator errors value

        Invalid error ->
            Validator (error :: errors) value


begin : a -> Validator a
begin value =
    Validator [] value


extractErrors : Validator a -> List Error
extractErrors (Validator errors _) =
    List.reverse errors


validateFloat : String -> String -> Status
validateFloat name string =
    case String.toFloat string of
        Ok _ ->
            Valid

        Err _ ->
            Invalid <| Error name "This is not a valid number"


findError : String -> List Error -> Maybe Error
findError field list =
    case list of
        [] ->
            Nothing

        ((Error errorField msg) as error) :: rest ->
            if errorField == field then
                Just error
            else
                findError field rest
