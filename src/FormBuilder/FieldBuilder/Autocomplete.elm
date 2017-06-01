module FormBuilder.FieldBuilder.Autocomplete
    exposing
        ( AutocompleteAttributes
        , get
        , input
        , update
        , subscriptions
        , Msg
        , searchQuery
        , withLabels
        , withHover
        , State
        , autocompleteState
        , minChars
        , maxResults
        )

{-| Creates an autocomplete field, able to search accross the web from multiple sources.

@docs AutocompleteAttributes
@docs get
@docs input
@docs update
@docs subscriptions
@docs Msg
@docs searchQuery
@docs withLabels
@docs withHover
@docs State
@docs autocompleteState
@docs minChars
@docs maxResults

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http exposing (Error)
import Dict exposing (Dict)
import Time exposing (Time)
import Task
import Keyboard
import FormBuilder.FieldBuilder.Attributes as Attributes exposing (AttributesModifier, FieldAttributes)
import FormBuilder.FieldBuilder as FieldBuilder exposing (FieldView)
import Json.Decode as Json
import Helpers.List as List
import Elegant exposing (..)
import Elegant.Elements as Elements
import Color
import List.Extra as List
import Helpers.KeyCode as KeyCode
import Helpers.List as List


{-| -}
type alias AutocompleteAttributes element =
    { labels : Maybe (element -> String)
    , hoverStyle : List (Elegant.Style -> Elegant.Style)
    }


defaultAttributes : FieldAttributes (AutocompleteAttributes element) msg
defaultAttributes =
    { common = Attributes.commonAttributes
    , labels = Nothing
    , hoverStyle =
        [ Elegant.backgroundColor (Color.grayscale 0.1)
        , Elegant.textColor (Color.grayscale 0.9)
        ]
    }


{-| -}
withLabels :
    (element -> String)
    -> FieldAttributes (AutocompleteAttributes element) msg
    -> FieldAttributes (AutocompleteAttributes element) msg
withLabels labelFun attributes =
    { attributes | labels = Just labelFun }


{-| -}
withHover :
    List (Elegant.Style -> Elegant.Style)
    -> FieldAttributes (AutocompleteAttributes element) msg
    -> FieldAttributes (AutocompleteAttributes element) msg
withHover hoverStyle attributes =
    { attributes | hoverStyle = hoverStyle }


{-| -}
type State element msg
    = State
        { requests : List ( String, Json.Decoder (List element) )
        , fetchedElements : Dict String (List element)
        , elements : List element
        , selectedElement : Maybe Int
        , searchQuery : String
        , lastKeyboardActivity : Time
        , globalTime : Time
        , wrapperMsg : Msg (List element) -> msg
        , selectMsg : element -> msg
        , focused : Bool
        , timeBeforeBlur : Float
        , minChars : Int
        , maxResults : Maybe Int
        }


{-| -}
autocompleteState :
    (Msg (List element) -> msg)
    -> (element -> msg)
    -> List ( String, Json.Decoder (List element) )
    -> State element msg
autocompleteState wrapperMsg selectMsg requests =
    State
        { requests = requests
        , fetchedElements = Dict.empty
        , elements = []
        , selectedElement = Nothing
        , searchQuery = ""
        , lastKeyboardActivity = 0
        , globalTime = 0
        , wrapperMsg = wrapperMsg
        , selectMsg = selectMsg
        , focused = False
        , timeBeforeBlur = 0
        , minChars = 1
        , maxResults = Nothing
        }


{-| Set the minimum number of character before launch of requests.
It can't be less than 1.
-}
minChars : Int -> State element msg -> State element msg
minChars minChars_ ((State state_) as state) =
    if minChars_ < 1 then
        state
    else
        State { state_ | minChars = minChars_ }


{-| -}
maxResults : Int -> State element msg -> State element msg
maxResults maxResults_ ((State state_) as state) =
    if maxResults_ < 1 then
        state
    else
        State { state_ | maxResults = Just maxResults_ }


{-| -}
searchQuery : State element msg -> String
searchQuery (State { searchQuery }) =
    searchQuery


{-| -}
type Msg elements
    = UpdateSearchQuery String
    | UpdateGlobalTimeAndFetchRequests Float Time
    | HandleKeyboardActivity Time
    | HandleKeyboardPress Keyboard.KeyCode
    | HandleKeyboardUp Keyboard.KeyCode
    | HandleKeyboardDown Keyboard.KeyCode
    | HandleHttpResult String (Result Error elements)
    | FocusAutocomplete
    | BlurAutocomplete Time
    | DelayBlur
    | SelectElement Int
    | SelectActivity ()


{-| -}
update :
    Msg (List element)
    -> State element msg
    -> ( State element msg, Cmd msg )
update msg (State ({ wrapperMsg, selectMsg, selectedElement, elements, globalTime, requests, lastKeyboardActivity, searchQuery, minChars } as state)) =
    case msg of
        UpdateSearchQuery input ->
            (State { state | searchQuery = input }
                |> selectElements
            )
                ! []

        UpdateGlobalTimeAndFetchRequests delay time ->
            State { state | globalTime = time }
                ! [ if isElapsedDelay time lastKeyboardActivity delay || String.length searchQuery < minChars then
                        Cmd.none
                    else
                        fetchRequests requests searchQuery wrapperMsg
                  ]

        SelectElement index ->
            State { state | selectedElement = Just index } ! []

        HandleKeyboardActivity time ->
            State { state | lastKeyboardActivity = time } ! []

        HandleKeyboardPress keyCode ->
            State state ! []

        HandleKeyboardUp keyCode ->
            State state ! []

        HandleKeyboardDown keyCode ->
            if isSpecialKeyCode keyCode then
                updateSelectedElement keyCode (State state)
            else
                ( State state, updateLastKeyboardActivity wrapperMsg )

        HandleHttpResult searchQuery result ->
            case result of
                Err error ->
                    State state ! []

                Ok element ->
                    (State state
                        |> addElementInCache searchQuery element
                        |> selectElements
                    )
                        ! []

        FocusAutocomplete ->
            State { state | focused = True } ! []

        BlurAutocomplete _ ->
            State
                { state
                    | focused = False
                    , timeBeforeBlur = 0
                    , selectedElement = Nothing
                }
                ! []

        DelayBlur ->
            State { state | timeBeforeBlur = 100 } ! []

        SelectActivity _ ->
            State { state | focused = False, timeBeforeBlur = 0 }
                ! [ case selectedElement of
                        Nothing ->
                            Cmd.none

                        Just index ->
                            case (List.getAt index elements) of
                                Nothing ->
                                    Cmd.none

                                Just element ->
                                    Task.perform selectMsg (Task.succeed element)
                  ]


isSpecialKeyCode : Int -> Bool
isSpecialKeyCode keyCode =
    (keyCode == KeyCode.arrowUp)
        || (keyCode == KeyCode.arrowDown)
        || (keyCode == KeyCode.enter)


updateSelectedElement : Keyboard.KeyCode -> State element msg -> ( State element msg, Cmd msg )
updateSelectedElement keyCode (State ({ wrapperMsg, elements } as state)) =
    if List.length elements > 0 && isSpecialKeyCode keyCode then
        if keyCode == KeyCode.enter then
            ( State state, Task.perform (wrapperMsg << SelectActivity) (Task.succeed ()) )
        else
            ( State
                { state
                    | selectedElement =
                        changeSelectedElement
                            (State state)
                            (direction keyCode)
                }
            , Cmd.none
            )
    else
        ( State state, Cmd.none )


direction : Int -> Int
direction keyCode =
    if keyCode == KeyCode.arrowUp then
        -1
    else
        1


changeSelectedElement : State element msg -> Int -> Maybe Int
changeSelectedElement ((State { elements, selectedElement }) as state) offset =
    let
        elementsSize =
            elements |> List.length
    in
        if elementsSize == 0 then
            Nothing
        else
            Just
                (case selectedElement of
                    Nothing ->
                        if offset == 1 then
                            0
                        else
                            elementsSize - 1

                    Just selection ->
                        if selection == 0 && offset == -1 then
                            0
                        else if selection == (elementsSize - 1) && offset == 1 then
                            elementsSize - 1
                        else
                            selection + offset
                )


addElementInCache : String -> List element -> State element msg -> State element msg
addElementInCache searchQuery elements (State ({ fetchedElements, maxResults } as state)) =
    let
        elements_ =
            case maxResults of
                Nothing ->
                    elements

                Just max ->
                    List.take max elements

        updateElements element =
            Dict.insert searchQuery element fetchedElements
    in
        case Dict.get searchQuery fetchedElements of
            Nothing ->
                State { state | fetchedElements = updateElements elements_ }

            Just fetchedElements_ ->
                if (List.containsAll elements_ fetchedElements_) then
                    State state
                else
                    State { state | fetchedElements = updateElements (List.merge elements_ fetchedElements_) }


selectElements : State element msg -> State element msg
selectElements (State ({ searchQuery, fetchedElements } as state)) =
    State
        { state
            | elements =
                fetchedElements
                    |> Dict.get searchQuery
                    |> Maybe.withDefault []
            , selectedElement = Nothing
        }


updateLastKeyboardActivity : (Msg element -> msg) -> Cmd msg
updateLastKeyboardActivity msg =
    Task.perform (msg << HandleKeyboardActivity) Time.now


isElapsedDelay : Float -> Float -> Float -> Bool
isElapsedDelay globalTime lastKeyboardActivity delay =
    globalTime - lastKeyboardActivity < (delay * Time.millisecond)


fetchRequests :
    List ( String, Json.Decoder (List element) )
    -> String
    -> (Msg (List element) -> msg)
    -> Cmd msg
fetchRequests requests searchQuery msg =
    let
        toHttpRequest ( url, decoder ) =
            issueRequest searchQuery msg url decoder
    in
        requests
            |> List.map toHttpRequest
            |> Cmd.batch


issueRequest :
    String
    -> (Msg (List element) -> msg)
    -> String
    -> Json.Decoder (List element)
    -> Cmd msg
issueRequest parameter msg url =
    Http.send
        (msg << HandleHttpResult parameter)
        << Http.get
            (url ++ (Http.encodeUri parameter))


toLabeledList : (element -> String) -> ( Int, element ) -> ( String, ( Int, element ) )
toLabeledList getLabel element =
    let
        label =
            getLabel (Tuple.second element)
    in
        ( label, element )


removeLabels : List ( String, indexedElement ) -> List indexedElement
removeLabels elements =
    case elements of
        ( label, element ) :: tl ->
            element :: removeLabels tl

        [] ->
            []


groupByLabel : List ( String, indexedElement ) -> List ( String, List indexedElement )
groupByLabel =
    List.reverse << groupByLabelHelp []


groupByLabelHelp :
    List ( String, List indexedElement )
    -> List ( String, indexedElement )
    -> List ( String, List indexedElement )
groupByLabelHelp acc elements =
    case List.head elements of
        Just ( label, element ) ->
            let
                matchingElements =
                    elements
                        |> List.filter (\( label_, element_ ) -> label_ == label)
                        |> removeLabels

                others =
                    elements
                        |> List.filter (\( label_, element_ ) -> label_ /= label)
            in
                groupByLabelHelp (( label, matchingElements ) :: acc) others

        Nothing ->
            acc


elementView :
    List (Elegant.Style -> Elegant.Style)
    -> State element msg
    -> (element -> Html msg)
    -> ( Int, element )
    -> Html msg
elementView hoverStyle (State { selectMsg, selectedElement, wrapperMsg }) elementCellView ( index, element ) =
    let
        selected =
            case selectedElement of
                Nothing ->
                    False

                Just selected_ ->
                    selected_ == index

        computedHoverStyle =
            if selected then
                hoverStyle
            else
                []
    in
        Html.a
            [ Html.Events.onClick (selectMsg element)
            , Html.Events.onMouseEnter ((wrapperMsg << SelectElement) index)
            , Elegant.style (paddingHorizontal medium :: paddingVertical small :: cursorPointer :: displayBlock :: computedHoverStyle)
            ]
            [ elementCellView element ]


elementAndLabelView :
    List (Elegant.Style -> Elegant.Style)
    -> State element msg
    -> (element -> Html msg)
    -> ( String, List ( Int, element ) )
    -> Html msg
elementAndLabelView hoverStyle state elementCellView ( label, elements ) =
    Html.div []
        [ Html.div
            [ Elegant.style
                [ padding tiny
                , paddingLeft small
                , displayBlock
                , widthPercent 100
                , backgroundColor (Color.rgb 250 250 250)
                , Elements.borderBottom (Color.rgb 230 230 230)
                ]
            ]
            [ Html.text label ]
        , Html.div []
            (List.map (elementView hoverStyle state elementCellView) elements)
        ]


inputView : State element msg -> Maybe String -> Html msg
inputView (State { searchQuery, wrapperMsg, selectMsg, elements }) placeholder =
    Html.input
        (List.concat
            [ [ Html.Attributes.value searchQuery
              , Html.Attributes.type_ "text"
              , Html.Attributes.autocomplete False
              , Html.Events.onInput (wrapperMsg << UpdateSearchQuery)
              , Html.Attributes.tabindex 0
              , Html.Events.onFocus (wrapperMsg FocusAutocomplete)
              , Html.Events.onBlur (wrapperMsg DelayBlur)
              , Elegant.style [ fullWidth, padding tiny ]
              ]
            , case placeholder of
                Nothing ->
                    []

                Just placeholder_ ->
                    [ Html.Attributes.placeholder placeholder_ ]
            ]
        )
        []


dropdownView :
    Maybe (element -> String)
    -> List (Elegant.Style -> Elegant.Style)
    -> State element msg
    -> (element -> Html msg)
    -> Html msg
dropdownView labels hoverStyle ((State { searchQuery, wrapperMsg, selectMsg, elements }) as state) elementCellView =
    Html.div
        [ Elegant.style
            [ backgroundColor (Color.rgb 255 255 255)
            , positionAbsolute
            , overflowHidden
            , marginTop (Px -2)
            , widthPercent 100
            , Elements.border (Color.rgb 221 221 221)
            , borderBottomLeftRadius 7
            , borderBottomRightRadius 7
            , zIndex 10
            ]
        ]
        (let
            elements_ =
                List.toIndexedList elements
         in
            case labels of
                Just fun ->
                    elements_
                        |> List.map (toLabeledList fun)
                        |> groupByLabel
                        |> List.map (elementAndLabelView hoverStyle state elementCellView)

                Nothing ->
                    List.map (elementView hoverStyle state elementCellView) elements_
        )


{-| -}
subscriptions :
    State element msg
    -> Float
    -> Sub msg
subscriptions (State { globalTime, lastKeyboardActivity, focused, elements, timeBeforeBlur, wrapperMsg }) delay =
    Sub.batch
        [ if timeBeforeBlur > 0 then
            Time.every (timeBeforeBlur * Time.millisecond) (wrapperMsg << BlurAutocomplete)
          else
            Sub.none
        , if List.isEmpty elements && isElapsedDelay globalTime lastKeyboardActivity delay then
            Time.every (delay * Time.millisecond) (wrapperMsg << UpdateGlobalTimeAndFetchRequests delay)
          else
            Sub.none
        , if focused then
            Sub.batch
                [ Keyboard.presses (wrapperMsg << HandleKeyboardPress)
                , Keyboard.ups (wrapperMsg << HandleKeyboardUp)
                , Keyboard.downs (wrapperMsg << HandleKeyboardDown)
                ]
          else
            Sub.none
        ]


{-| -}
input :
    State element msg
    -> (element -> Html msg)
    -> List (AttributesModifier (AutocompleteAttributes element) msg)
    -> Html msg
input autocomplete_ elementCellView =
    FieldBuilder.object defaultAttributes (Just (view autocomplete_ elementCellView))


view :
    State element msg
    -> (element -> Html msg)
    -> FieldAttributes (AutocompleteAttributes element) msg
    -> Html msg
view ((State { searchQuery, wrapperMsg, selectMsg, elements, focused }) as state) elementCellView { common, labels, hoverStyle } =
    Html.div
        [ Html.Attributes.style [ ( "position", "relative" ) ] ]
        [ Html.div []
            [ inputView state common.placeholder ]
        , if List.length elements > 0 && focused then
            dropdownView labels hoverStyle state elementCellView
          else
            Html.text ""
        ]


{-| -}
get : String -> Json.Decoder element -> ( String, Json.Decoder element )
get uri decoder =
    ( uri, decoder )


compose : List (a -> a) -> a -> a
compose =
    List.foldr (>>) identity
