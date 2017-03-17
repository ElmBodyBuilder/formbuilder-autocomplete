module FormBuilder.Autocomplete exposing (default)

{-| View of the autocomplete field, used to render it in the browser.

@docs default
-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import FormBuilder.FieldBuilder.Attributes as Attributes exposing (FieldAttributes, AttributesModifier)
import FormBuilder.FieldBuilder.Events as Events
import FormBuilder.FieldBuilder as FieldBuilder exposing (FieldView)
import FormBuilder.Autocomplete.Attributes as Attributes exposing (AutocompleteAttributes, defaultAttributes)


{-| Default field of the autocomplete.
-}
default : List (AttributesModifier (AutocompleteAttributes a msg) msg) -> Html msg
default =
    FieldBuilder.object defaultAttributes (Just inputField)


choicesView : List a -> (a -> String) -> (a -> msg) -> Maybe a -> List (Html msg)
choicesView choices choiceView onSelect selected =
    choices |> List.map (elementView choiceView onSelect selected)


elementView : (a -> String) -> (a -> msg) -> Maybe a -> a -> Html msg
elementView choiceView onSelect selected choices =
    let
        description =
            choiceView choices
    in
        Html.a
            [ Html.Attributes.style
                (List.append
                    [ ( "cursor", "pointer" )
                    , ( "padding", "3px" )
                    , ( "display", "block" )
                    , ( "width", "100%" )
                    , ( "padding-left", "12px" )
                    ]
                    (if selected == Just choices then
                        [ ( "background-color", "#28DBD2" )
                        , ( "color", "#fff" )
                        ]
                     else
                        []
                    )
                )
            , Html.Events.onMouseEnter
                (onSelect choices)
            ]
            [ Html.text (description) ]


inputField : FieldView (AutocompleteAttributes a msg) msg
inputField attributes =
    case attributes.choiceView of
        Nothing ->
            Html.text ""

        Just choiceView ->
            case attributes.onSelect of
                Nothing ->
                    Html.text ""

                Just click ->
                    case attributes.searchQuery of
                        Nothing ->
                            Html.text ""

                        Just text ->
                            display
                                (case attributes.choices of
                                    Nothing ->
                                        []

                                    Just choices ->
                                        choices
                                )
                                choiceView
                                click
                                text
                                attributes


display :
    List ( String, List a )
    -> (a -> String)
    -> (a -> msg)
    -> String
    -> FieldAttributes (AutocompleteAttributes a msg) msg
    -> Html msg
display choices choiceView onSelect searchQuery attributes =
    case attributes.common.onInput of
        Just event ->
            case attributes.common.placeholder of
                Nothing ->
                    Html.text ""

                Just placeholder ->
                    Html.div
                        [ Html.Attributes.style
                            [ ( "position", "relative" )
                            ]
                        ]
                        [ FieldBuilder.object
                            attributes
                            Nothing
                            [ Attributes.type_ Attributes.Text
                            , Attributes.placeholder placeholder
                            , Attributes.autocomplete False
                            , Attributes.searchQuery searchQuery
                            , Attributes.value searchQuery
                            , Events.onInput event
                            ]
                        , if (searchQuery |> String.length) < 5 || not attributes.focused then
                            Html.text ""
                          else
                            Html.div
                                [ Html.Attributes.style
                                    [ ( "border-bottom-left-radius", "7px" )
                                    , ( "border-bottom-right-radius", "7px" )
                                    , ( "overflow", "hidden" )
                                    , ( "border", "1px solid #ddd" )
                                    , ( "border-top", "none" )
                                    , ( "position", "absolute" )
                                    , ( "margin-top", "-2px" )
                                    , ( "width", "100%" )
                                    , ( "background-color", "#ffffff" )
                                    , ( "z-index", "10" )
                                    ]
                                ]
                                (choices |> List.map (placeLabel choiceView onSelect attributes.selectedElement))
                        ]

        Nothing ->
            Html.text ""


placeLabel : (a -> String) -> (a -> msg) -> Maybe a -> ( String, List a ) -> Html msg
placeLabel choiceView onSelect selected choices =
    Html.div []
        (if (List.length (Tuple.second choices)) == 0 then
            []
         else
            (List.append
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "padding", "2px" )
                        , ( "padding-left", "6px" )
                        , ( "display", "block" )
                        , ( "width", "100%" )
                        , ( "background-color", "#fbfbfb" )
                        , ( "font-size", "80%" )
                        , ( "border-bottom", "1px solid #ddd" )
                        , ( "border-top", "1px solid #ddd" )
                        , ( "color", "#888" )
                        ]
                    ]
                    [ Html.text (Tuple.first choices) ]
                ]
                (choicesView (Tuple.second choices) choiceView onSelect selected)
            )
        )
