module Prototype exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html, sub)
import Html.Attributes as A
import Html.Events as Ev
import Json.Decode as Json


type alias Model =
    { tree : Token
    , keyboard : Keyboard
    , actions : List Action
    }


type Keyboard
    = Editing String TokenPath
    | Navigating TokenPath


type TokenPath
    = This
    | Next Int TokenPath


type Token
    = Token Variant
    | Group String Layout (List Token)


type Variant
    = Syntax String
    | Indent Token
    | SubToken String


token : String -> Token
token s =
    Token (SubToken s)


tokenWith : String -> List Token -> Token
tokenWith s xs =
    Group s Hori xs


indent : List Token -> Token
indent xs =
    Token (Indent (group Vert xs))


syntax : String -> Token
syntax s =
    Token (Syntax s)


group : Layout -> List Token -> Token
group l xs =
    Group "" l xs


type Layout
    = Vert
    | Hori


init : () -> ( Model, Cmd Action )
init _ =
    ( { tree =
            group Vert
                [ group Hori
                    [ token "State", syntax ":" ]
                , group Vert
                    [ syntax "{"
                    , indent <|
                        [ group Hori
                            [ token "session"
                            , syntax " : "
                            , token "Session"
                            ]
                        , group Hori
                            [ token "problems"
                            , syntax " : "
                            , tokenWith "List" [ token "Problem" ]
                            ]
                        , group Hori
                            [ token "form"
                            , syntax " : "
                            , tokenWith "Maybe" [ token "Form" ]
                            ]
                        ]
                    , syntax "}"
                    ]
                , group Vert
                    [ group Hori
                        [ token "view"
                        , syntax ":"
                        , token "State"
                        , syntax "->"
                        , tokenWith "Elem" [ token "Event" ]
                        ]
                    , group Hori
                        [ token "view"
                        , syntax " = \\ "
                        , token "state"
                        , syntax "->"
                        ]
                    , indent <|
                        [ group Hori
                            [ token "content"
                            , syntax "="
                            , tokenWith "col"
                                [ group Hori [ syntax "{", syntax "}" ]
                                , group Vert
                                    [ syntax "["
                                    , indent [ token "a" ]
                                    , syntax "]"
                                    ]
                                ]
                            ]
                        , group Hori
                            [ syntax "{"
                            , group Hori
                                [ token "title"
                                , syntax ":"
                                , token "\"Register\""
                                , syntax ","
                                ]
                            , token "content"
                            , syntax "}"
                            ]
                        ]
                    ]
                ]
      , keyboard = Navigating This
      , actions = []
      }
    , Cmd.none
    )


view : Model -> Html Action
view model =
    H.div [ A.class "flex flex-col justify-start items-start" ] <|
        (case model.keyboard of
            Navigating path ->
                [ viewToken (Just path) This model.tree ]

            Editing _ path ->
                [ viewToken (Just path) This model.tree ]
        )
            ++ [ H.text <| Debug.toString model.keyboard ]


viewToken : Maybe TokenPath -> TokenPath -> Token -> Html Action
viewToken selPath path token_ =
    let
        ( self, sty, xs) =
            case token_ of
                Group name lay xs_ ->
                    (H.text name, tokenStyles (isSel selPath) lay, xs_)
                Token (SubToken var) -> 
                    (H.text var, tokenStyles (isSel selPath) Hori, [])
                Token (Syntax var) -> 
                    (H.text var, noStyle, [])
                Token (Indent tok) -> 
                    (viewToken selPath path tok, A.class "ml-4", [])
               
        viewSubtoken idx subtoken =
            viewToken
                (case selPath of
                    Just (Next n subpath) ->
                        if n == idx then
                            Just subpath

                        else
                            Nothing

                    _ ->
                        Nothing
                )
                (appendPath idx path)
                subtoken

        appendPath idx pth =
            case pth of
                This ->
                    Next idx This

                Next n subPath ->
                    Next n (appendPath idx subPath)

        subtokens =
            List.indexedMap viewSubtoken xs
    in
    H.div
        [ sty
        , Ev.onMouseEnter (UserMouseHover path)
        ]
    <|
        self
            :: subtokens


none : H.Html msg 
none = H.text ""

noStyle : H.Attribute msg
noStyle = A.classList []

isSel : Maybe TokenPath -> Bool
isSel mpath =
    case mpath of
        Just This ->
            True

        _ ->
            False


tokenStyles : Bool -> Layout -> H.Attribute msg
tokenStyles sel layout =
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
            , if sel then
                "border-red-500"

              else
                "border-gray-200"
            , if sel then
                "dark:border-red-500"

              else
                "dark:border-gray-700"
            , "cursor-pointer"
            , case layout of
                Hori ->
                    "flex flex-row items-center"

                Vert ->
                    ""
            ]


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        UserMouseHover path ->
            { model
                | keyboard = Navigating path
            }
                |> state

        MoveDown ->
            { model
                | keyboard = mapKeyboard (down model.tree) model.keyboard
            }
                |> state

        MoveUp ->
            { model
                | keyboard = mapKeyboard (up model.tree) model.keyboard
            }
                |> state

        MoveLeft ->
            { model
                | keyboard = mapKeyboard (left model.tree) model.keyboard
            }
                |> state

        MoveRight ->
            { model
                | keyboard = mapKeyboard (right model.tree) model.keyboard
            }
                |> state

        _ ->
            state model


down : Token -> TokenPath -> TokenPath
down t tp =
    Debug.todo "DOWN"


up : Token -> TokenPath -> TokenPath
up xs p =
    Debug.todo "Up"


left : Token -> TokenPath -> TokenPath
left xs p =
    Debug.todo "left"


right : Token -> TokenPath -> TokenPath
right xs p =
    Debug.todo "RIGHT"


type Action
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | Escape
    | Enter
    | Space
    | Type Int
    | UserMouseHover TokenPath


action : Action -> Token -> TokenPath -> TokenPath
action act_ tok tp =
    tp


{-| Transforms keyboard signals into our own language
There should be modifiers and whatnots here
-}
keyToAction : Int -> Action
keyToAction k =
    case k of
        40 ->
            MoveDown

        39 ->
            MoveRight

        37 ->
            MoveLeft

        38 ->
            MoveUp

        27 ->
            Escape

        13 ->
            Enter

        32 ->
            Space

        _ ->
            Type k


mapKeyboard : (TokenPath -> TokenPath) -> Keyboard -> Keyboard
mapKeyboard fn key =
    case key of
        Navigating tp ->
            Navigating (fn tp)

        Editing s tp ->
            Editing s (fn tp)


state : Model -> ( Model, Cmd Action )
state m =
    ( m, Cmd.none )


main : Program () Model Action
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
                            |> Json.map
                                keyToAction
                        )
                    ]
        }
