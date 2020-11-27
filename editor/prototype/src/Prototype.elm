module Prototype exposing (..)

import Browser
import Browser.Events
import Html as H exposing (Html, sub)
import Html.Attributes as A
import Html.Events as Ev
import Json.Decode as Json
import List
import List.Extra as ListE
import Maybe.Extra exposing (orList)


type alias Model =
    { tree : Token
    , keyboard : Keyboard
    , actions : List Msg
    , token : Maybe Token
    }


type Keyboard
    = Editing String TokenPath
    | Navigating TokenPath


type TokenPath
    = End
    | Next Int TokenPath


type Token
    = Atom String
    | Syntax String
    | Group String Layout (List Token)


type Layout
    = Vert
    | Hori


type alias TokenPast =
    { sibilings : List Int
    , current : Int
    , layout : Layout
    }


type ResolvePath
    = BackPath
    | NewPath TokenPath


move : Direction -> Token -> TokenPath -> TokenPath
move dir token_ path =
    moveStep dir token_ path { sibilings = [ 0 ], current = 0, layout = Vert }


moveStep : Direction -> Token -> TokenPath -> TokenPast -> TokenPath
moveStep dir tok path { sibilings, current, layout } =
    case path of
        End ->
            -- root case
            moves tok path sibilings current layout (moveSteps ( dir, layout ))
                |> Maybe.withDefault End

        Next n End ->
            -- step case
            case tok of
                Group _ l xs ->
                    ListE.getAt n xs
                        |> Maybe.andThen
                            (\t ->
                                moves t path (selectable xs) n l (moveSteps ( dir, l ))
                                  
                            )
                        |> Maybe.withDefault End

                _ ->
                    End

        Next n sub ->
            -- recursive step
            case tok of
                Group _ l xs ->
                    ListE.getAt n xs
                        |> Maybe.map
                            (\t ->
                                Next n (moveStep dir t sub { sibilings = selectable xs, current = n, layout = l })
                            )
                        |> Maybe.withDefault End

                _ ->
                    End


moveSteps : ( Direction, Layout ) -> List Move
moveSteps dl =
    case  dl of
        ( Up, Vert ) ->
            [ PrevSibiling
            , Parent
            , LastChild
            ]

        ( Up, Hori ) ->
            [ Parent 
            , PrevSibiling
            , LastChild
            ]

        ( Down, Vert ) ->
            [ NextSibiling
            , Parent
            , FirstChild
            ]

        ( Down, Hori ) ->
            [ FirstChild
            , Parent
            , NextSibiling
            ]

        ( Left, Vert ) ->
            [ Parent
            , PrevSibiling
            , LastChild
            ]

        ( Left, Hori ) ->
            [ PrevSibiling
            , Parent
            , LastChild
            ]

        ( Right, Hori ) ->
            [ NextSibiling
            , FirstChild
            , Parent
            ]

        ( Right, Vert ) ->
            [ FirstChild
            , NextSibiling
            , Parent
            ]

        ( Inside, _ ) ->
            [ FirstChild
            , NextSibiling
            , Parent
            ]

        ( Outside, _ ) ->
            [ Parent
            , PrevSibiling
            , LastChild
            ]


type Move
    = Parent
    | NextSibiling
    | PrevSibiling
    | FirstChild
    | LastChild


moves : Token -> TokenPath -> List Int -> Int -> Layout -> List Move -> Maybe TokenPath
moves tok path sibilings current layout mvs =
    case tok of
        Group _ l toks ->
            mvs
                |> List.map
                    (\m ->
                        case m of
                            Parent ->
                                parent current sibilings

                            NextSibiling ->
                                nextSibiling current sibilings

                            PrevSibiling ->
                                prevSibiling current sibilings

                            FirstChild ->
                                firstChild current toks path

                            LastChild ->
                                lastChild current toks path
                    )
                |> orList

        _ ->
            mvs
                |> List.map
                    (\m ->
                        case m of
                            Parent ->
                                parent current sibilings

                            NextSibiling ->
                                nextSibiling current sibilings

                            PrevSibiling ->
                                prevSibiling current sibilings

                            _ ->
                                Nothing
                    )
                |> orList


nextSibiling : Int -> List Int -> Maybe TokenPath
nextSibiling n xs =
    ListE.find (\i -> i > n) xs
        |> Maybe.map selectedChild


prevSibiling : Int -> List Int -> Maybe TokenPath
prevSibiling n xs =
    List.filter (\i -> i < n) xs
        |> ListE.last
        |> Maybe.map selectedChild


firstChild : Int -> List Token -> TokenPath -> Maybe TokenPath
firstChild current its p =
    selectable its
        |> List.head
        |> Maybe.map (\i -> appendPath i p)


selectedChild : Int -> TokenPath
selectedChild n =
    Next n End


lastChild : Int -> List Token -> TokenPath -> Maybe TokenPath
lastChild current its p =
    selectable its
        |> ListE.last
        |> Maybe.map (\i -> appendPath i p)


parent : Int -> List Int -> Maybe TokenPath
parent n xs =
    if List.length xs < 2 then
        Nothing

    else
        Just End


getTokenAtPath : TokenPath -> Token -> Token
getTokenAtPath p tok =
    case p of
        End ->
            tok

        Next n sub ->
            case tok of
                Group _ l xs ->
                    ListE.getAt n xs
                        |> Maybe.map (getTokenAtPath sub)
                        |> Maybe.withDefault tok

                _ ->
                    tok


selectable : List Token -> List Int
selectable ts =
    ListE.findIndices isSelectable ts


isSelectable : Token -> Bool
isSelectable t =
    case t of
        Syntax _ ->
            False

        _ ->
            True


type Direction
    = Down
    | Up
    | Left
    | Right
    | Inside
    | Outside


token : String -> Token
token s =
    Atom s


tokenWith : String -> List Token -> Token
tokenWith s xs =
    Group s Hori xs


syntax : String -> Token
syntax s =
    Syntax s


group : Layout -> List Token -> Token
group l xs =
    Group "" l xs


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree =
            group Vert
                [ testState
                , testViewFn
                ]
      , keyboard = Navigating (Next 0 End)
      , actions = []
      , token = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    H.div [ A.class "flex flex-col justify-start items-start" ] <|
        H.p [ A.class "my-4 text-lg" ] [ H.text intro ]
            :: (case model.keyboard of
                    Navigating path ->
                        [ viewToken (Just path) End model.tree ]

                    Editing _ path ->
                        [ viewToken (Just path) End model.tree ]
               )


intro =
    "Welcome to Prototype :) Use Arrow keys to navigate. Spacebar enters in a group of tokens. Escape goes level up"



-- ++ [ H.text <| Debug.toString model.keyboard
--    , H.div [] [ H.text <| Debug.toString model.token ]
--    ]


viewToken : Maybe TokenPath -> TokenPath -> Token -> Html Msg
viewToken selPath path token_ =
    let
        ( self, sty, xs ) =
            case token_ of
                Group name lay xs_ ->
                    ( H.text name, tokenStyles (isSel selPath) lay name, xs_ )

                Atom var ->
                    ( H.text var, tokenStyles (isSel selPath) Hori var, [] )

                Syntax s ->
                    ( H.text s, noStyle, [] )

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

        subtokens =
            List.indexedMap viewSubtoken xs
    in
    H.div
        [ sty
        , Ev.onMouseEnter (UserMouseHover token_ path)
        ]
    <|
        self
            :: subtokens


appendPath : Int -> TokenPath -> TokenPath
appendPath idx pth =
    case pth of
        End ->
            Next idx End

        Next n subPath ->
            Next n (appendPath idx subPath)


none : H.Html msg
none =
    H.text ""


noStyle : H.Attribute msg
noStyle =
    A.class "text-gray-500 dark:text-gray-100"


isSel : Maybe TokenPath -> Bool
isSel mpath =
    case mpath of
        Just End ->
            True

        _ ->
            False


tokenStyles : Bool -> Layout -> String -> H.Attribute msg
tokenStyles sel layout name =
    A.class <|
        String.join " "
            [ "px-2"
            , "py-1"
            , "my-1"
            , "bg-gray-100"
            , "dark:bg-gray-800"
            , if
                String.uncons name
                    |> Maybe.map (\( c, _ ) -> Char.isUpper c)
                    |> Maybe.withDefault False
              then
                "text-green-500"

              else
                "text-blue-800 dark:text-green-100"
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserMouseHover tok path ->
            { model
                | keyboard = Navigating path
                , token = Just tok
            }

        Move dir ->
            { model
                | keyboard = mapKeyboard (move dir model.tree) model.keyboard
            }

        _ ->
            model


type Msg
    = Move Direction
    | Escape
    | Enter
    | Space
    | Type Int
    | UserMouseHover Token TokenPath


{-| Transforms keyboard signals into our own language
There should be modifiers and whatnots here
-}
keyToAction : Int -> Msg
keyToAction k =
    case k of
        40 ->
            Move Down

        39 ->
            Move Right

        37 ->
            Move Left

        38 ->
            Move Up

        27 ->
            Move Outside

        13 ->
            Enter

        32 ->
            Move Inside

        _ ->
            Type k


mapKeyboard : (TokenPath -> TokenPath) -> Keyboard -> Keyboard
mapKeyboard fn key =
    case key of
        Navigating tp ->
            Navigating (fn tp)

        Editing s tp ->
            Editing s (fn tp)


state : Model -> ( Model, Cmd Msg )
state m =
    ( m, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> state (update msg model)
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


testState : Token
testState =
    group Vert
        [ group Hori
            [ token "State", syntax ":" ]
        , group Vert
            [ syntax "{"
            , group Hori
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
            , syntax "}"
            ]
        ]


testViewFn : Token
testViewFn =
    group Vert
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
        , group Vert
            [ group Hori
                [ token "content"
                , syntax "="
                , tokenWith "col"
                    [ group Hori [ syntax "{", syntax "}" ]
                    , group Vert
                        [ syntax "["
                        , token "a"
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
