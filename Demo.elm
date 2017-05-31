module Demo exposing (..)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import FormBuilder.FieldBuilder.Autocomplete as Autocomplete


type Msg
    = Autocomplete (Autocomplete.Msg Quotes)
    | SelectElement Quote


type alias Quote =
    { iconUrl : String
    , id : String
    , url : String
    , value : String
    }


type alias Quotes =
    List Quote


type alias Model =
    { autocompleteState : Autocomplete.State Quote Msg }


decodeQuotes : Decoder Quotes
decodeQuotes =
    Decode.at [ "result" ] <|
        Decode.list decodeQuote


decodeQuote : Decoder Quote
decodeQuote =
    Decode.map4 Quote
        (Decode.field "icon_url" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "value" Decode.string)


asAutocompleteState : Model -> Autocomplete.State Quote Msg -> Model
asAutocompleteState model autocompleteState =
    { model | autocompleteState = autocompleteState }


init : ( Model, Cmd Msg )
init =
    { autocompleteState =
        Autocomplete.autocompleteState
            Autocomplete
            SelectElement
            [ Autocomplete.get "https://api.chucknorris.io/jokes/search?query=" decodeQuotes ]
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ autocompleteState } as model) =
    case msg of
        Autocomplete msg_ ->
            let
                ( state, cmds ) =
                    Autocomplete.update msg_ autocompleteState
            in
                (state |> asAutocompleteState model) ! [ cmds ]

        SelectElement quote ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions { autocompleteState } =
    Sub.batch [ Autocomplete.subscriptions autocompleteState 250 Autocomplete ]


cellView : Quote -> Html Msg
cellView { value } =
    Html.text value


view : Model -> Html Msg
view { autocompleteState } =
    Html.div []
        [ Autocomplete.input
            autocompleteState
            cellView
            []
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
