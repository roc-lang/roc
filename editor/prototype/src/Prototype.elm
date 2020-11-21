module Prototype exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Ev
import Json.Decode as Json


type alias Model =
    { tree : Token
    , keyCode : Maybe Int
    }


type Token
    = Token String
    | Syntax String
    | SubToken String
    | TokenGroup Layout (List Token)

type Selected
    = Nop
    | Node Int Selected

type Layout
    = Vert
    | Hori


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree =
            TokenGroup Vert
                [ TokenGroup Hori
                    [ Token "State", Syntax ":" ]
                , TokenGroup Vert
                    [ TokenGroup Hori
                        [ Token "session"
                        , Syntax " : "
                        , Token "Session"
                        ]
                    , TokenGroup Hori
                        [ Token "problems"
                        , Syntax " : "
                        , TokenGroup Hori
                            [ SubToken "List", Token "Problem" ]
                        ]
                    , TokenGroup Hori
                        [ Token "form"
                        , Syntax " : "
                        , TokenGroup Hori
                            [ SubToken "Maybe", Token "Form" ]
                        ]
                    ]
                ]
      , keyCode = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    H.div [ A.class "flex flex-col items-start" ]
        [ token model.tree
        , H.div [] [ H.text (Debug.toString model.keyCode) ]
        ]


tokenGroup : List Token -> Html Msg
tokenGroup xs =
    H.div
        []
        (List.map token xs)


token : Token -> Html Msg
token t =
    case t of
        Token s ->
            H.div
                [ tokenStyles                
                ]
                [ H.text s ]

        SubToken s ->
            H.div
                [ A.class "text-gray-900 dark:text-green-100 mr-2"
                ]
                [ H.text s ]

        Syntax s ->
            H.div
                [ A.class "mx-1 py-1"
                , A.class "text-gray-400 dark:text-gray-600"
                ]
                [ H.text s ]

        TokenGroup lay xs ->
            H.div
                [ A.class <|
                    case lay of
                        Vert ->
                            ""

                        Hori ->
                            "flex flex-row items-baseline"
                , tokenStyles
                ]
                (List.map token xs)


tokenStyles : H.Attribute msg
tokenStyles =
    A.class <|
        String.join " "
            [ "px-2"
            , "py-1"
            , "my-1"
            , "bg-gray-100"
            , "dark:bg-gray-800"
            , "text-gray-900"
            , "dark:text-green-100"
            , "rounded-md"
            , "border-solid"
            , "border"
            , "border-gray-200"
            , "dark:border-gray-700"
            , "cursor-pointer"
            ]


type Msg
    = KeyDown Int
    | KeyUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            { model
                | keyCode = Just keyCode
            }
                |> state

        KeyUp ->
            { model | keyCode = Nothing }
                |> state


state : Model -> ( Model, Cmd Msg )
state m =
    ( m, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onKeyDown
                        (Ev.keyCode
                            |> Json.andThen
                                (KeyDown >> Json.succeed)
                        )
                    , Browser.Events.onKeyUp
                        (Json.succeed KeyUp)
                    ]
        }
