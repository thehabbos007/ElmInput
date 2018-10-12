-- Skrevet af: Ahmad Sattar
-- Denne del er kodet udelukkende i Elm
-- Heltal: Int
-- Tekst: String
-- Initialisering af programmets reference. Port specificerer at der kommer kontakt mellem Js og Elm


port module Main exposing (..)

--Imports til forskellige libraries

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)


-- Det der svarer til main i c++/java osv.


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions --\_ -> Sub.none
        , view = view
        }



-- Modellens struktur. Her er alle andre alias typer det der bruges til at definere modellen


type alias Submission =
    { name : String
    , sum : Int
    }


type alias Model =
    { name : String
    , twoDigit : Int
    , threeDigit : Int
    , sum : Int
    , submissions : Maybe (List Submission)
    }


model : Model
model =
    Model "" 0 0 0 Nothing



-- Init er startsværdien til programmets state


init =
    ( model, Cmd.none )



-- Update funktionen. Den kaldes for hver Msg der kommer fra programmet. Men kun ved disse kald nedenfor


type Msg
    = GotMessageFromJs (List Submission)
    | Name String
    | TwoDigitChange String
    | ThreeDigitChange String
    | CalcSum



-- Denne funktion kaldes hver gang, og pattern matcher på forskellige funktioner.
-- Det er altså her alt kommunikationslogik sker


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessageFromJs list ->
            ( { model | submissions = Just (Debug.log "Fra JS til Elm - " list) }, Cmd.none )

        Name name ->
            ( { model | name = name }, Cmd.none )

        TwoDigitChange nb ->
            case String.toInt nb of
                Ok val ->
                    ( { model | twoDigit = val, sum = val + model.threeDigit }, Cmd.none )

                Err val ->
                    ( model, Cmd.none )

        ThreeDigitChange nb ->
            case String.toInt nb of
                Ok val ->
                    ( { model | threeDigit = val, sum = model.twoDigit + val }, Cmd.none )

                Err val ->
                    ( model, Cmd.none )

        CalcSum ->
            ( { model | sum = model.twoDigit + model.threeDigit }, Cmd.none )



-- Subscriptions er hvor forskellige data som er uafhænhigt af programmet selv sker.
-- Det kunne være muse-events, vindue størrelses ændringer og i dette tilfælde kontakt ude til javascript funktioner


port localStorageJs : (List Submission -> msg) -> Sub msg


subscriptions model =
    localStorageJs GotMessageFromJs



-- View er en funktion, som når den får en model, giver alt visuelt i programmet.
-- Alt visuelt i programmet er altså simpelthen bare en funktion. Funktionerne kan sammenhænges


zeroOrBlankString : Int -> String
zeroOrBlankString num =
    if num == 0 then
        ""
    else
        toString num


lengthOfInt : Int -> Int
lengthOfInt int =
    int
        |> toString
        |> String.length


viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( lengthTwo, lengthThree ) =
            ( lengthOfInt model.twoDigit, lengthOfInt model.threeDigit )

        totalLength =
            lengthTwo + lengthThree

        ( color, message ) =
            if totalLength == 2 then
                ( "red", "2-cifre tallet skal være 2 cifre og det 3-cifrede tal skal 3 cifre" )
            else if lengthTwo /= 2 then
                ( "red", "2-cifre tallet skal være 2 cifre" )
            else if lengthThree /= 3 then
                ( "red", "3-cifre tallet skal være 3 cifre" )
            else
                ( "green", "Det ser fint ud" )
    in
    div [ style [ ( "color", color ) ] ] [ text message ]


eachItem : Submission -> Html Msg
eachItem item =
    li []
        [ button
            [ class "slet-en"
            , attribute "onclick" "deleteElement(this);"
            , attribute "data-id" item.name
            ]
            [ text "x" ]
        , text (" Navn: " ++ item.name ++ "  -  Sum: " ++ toString item.sum)
        ]


localStorageTable : Model -> List (Html Msg)
localStorageTable model =
    case model.submissions of
        Just list ->
            List.map eachItem list

        Nothing ->
            [ li [] [ text "Ingen data" ] ]


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", id "name", placeholder "Navn", onInput Name ] []
        , input [ type_ "number", placeholder "2-cifre", value (zeroOrBlankString model.twoDigit), onInput TwoDigitChange, H.min "10", H.max "99" ] []
        , input [ type_ "number", placeholder "3-cifre", value (zeroOrBlankString model.threeDigit), onInput ThreeDigitChange, H.min "100", H.max "999" ] []
        , input [ type_ "number", id "sum", placeholder "Sum", value (zeroOrBlankString model.sum), disabled True ] []
        , viewValidation model
        , ul [] (localStorageTable model)
        ]
