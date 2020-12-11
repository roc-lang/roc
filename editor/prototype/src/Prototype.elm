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
    , shiftDown : Bool
    , actions : List Msg
    , token : Maybe Token
    }


type Keyboard
    = Editing Caret TokenPath
    | Navigating TokenPath


type alias Caret =
    { before : String
    , after : String
    , shiftDown : Bool
    , selecte : String
    }


type TokenPath
    = End
    | Next Int TokenPath


type alias PathToParent =
    TokenPath


type Token
    = Atom Kind
    | Group Kind Layout (List Token)


type Kind
    = Empty
    | TypeName String
    | Syntax String
    | TagName String
    | FieldName String
    | ParamName String
    | FnName String
    | VarName String
    | StrVal String
    | NumVal String


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
    case dl of
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


type Direction
    = Down
    | Up
    | Left
    | Right
    | Inside
    | Outside


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
                                firstChild toks path

                            LastChild ->
                                lastChild toks path
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


firstChild : List Token -> TokenPath -> Maybe TokenPath
firstChild its p =
    selectable its
        |> List.head
        |> Maybe.map (\i -> appendPath i p)


selectedChild : Int -> TokenPath
selectedChild n =
    Next n End


lastChild : List Token -> TokenPath -> Maybe TokenPath
lastChild its p =
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
        Atom (Syntax _) ->
            False

        _ ->
            True


token : Kind -> Token
token =
    Atom


tokenWith : Kind -> List Token -> Token
tokenWith s xs =
    Group s Hori xs


syntax : String -> Token
syntax s =
    Atom (Syntax s)


group : Layout -> List Token -> Token
group l xs =
    Group Empty l xs


empty : Token
empty =
    token Empty


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree =
            treeRoot
                [ token Empty
                ]
      , keyboard = Editing (Caret "" "" False "") (Next 0 End)
      , shiftDown = False
      , actions = []
      , token = Nothing
      }
    , Cmd.none
    )


treeRoot : List Token -> Token
treeRoot =
    group Vert


view : Model -> Html Msg
view model =
    H.div [ A.class "flex flex-col justify-start items-start mx-16" ] <|
        H.p [ A.class "my-4 text-lg" ] [ H.text intro ]
            :: (case model.keyboard of
                    Navigating path ->
                        [ viewToken Nothing (Just path) End model.tree ]

                    Editing caret path ->
                        [ viewToken (Just caret) (Just path) End model.tree
                        ]
               )


intro : String
intro =
    "Welcome to Prototype :) Use Arrow keys to navigate. Spacebar enters in a group of tokens. Escape goes level up"



-- ++ [ H.text <| Debug.toString model.keyboard
--    , H.div [] [ H.text <| Debug.toString model.token ]
--    ]


viewToken : Maybe Caret -> Maybe TokenPath -> TokenPath -> Token -> Html Msg
viewToken mCaret selPath path token_ =
    let
        ( self, sty, xs ) =
            case token_ of
                Group name lay xs_ ->
                    ( H.text <| Debug.toString name, tokenStyles (isSel selPath) lay "name", xs_ )

                Atom (Syntax s) ->
                    ( H.text s, noStyle, [] )

                Atom k ->
                    case ( mCaret, selPath ) of
                        ( Just car, Just End ) ->
                            ( viewCaret car
                            , A.class "px-2 py-1"
                            , []
                            )

                        _ ->
                            ( H.text "var", tokenStyles (isSel selPath) Hori "var", [] )

        viewSubtoken idx subtoken =
            viewToken mCaret
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


viewCaret : Caret -> Html Msg
viewCaret caret =
    if String.isEmpty caret.before && String.isEmpty caret.after then
        H.span [ A.class "text-gray-600 font-italic flex flex-row items-baseline" ] [ caretEl, H.text "Expression" ]

    else
        H.span
            [ A.class "flex flex-row items-baseline"
            ]
            [ H.text caret.before, caretEl, H.text caret.after ]


caretEl : Html msg
caretEl =
    H.span [ A.class "inline-block w-px h-3 mx-px bg-pink-500 animate-pulse" ] []


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
                | token = Just tok
            }

        Keyboard event ->
            model


type alias Typing =
    ( Caret, TokenPath, Token )


onKeyboardEvent : KeyboardEvent -> Typing -> Typing
onKeyboardEvent ev ( c, path, tok ) =
    case ev of
        Char char ->
            ( { c
                | after =
                    String.append c.after
                        (String.fromChar <|
                            if c.shiftDown then
                                Char.toUpper char

                            else
                                char
                        )
              }
            , path
            , tok
            )

        Shift ->
            ( { c | shiftDown = True }, path, tok )

        LiftUp ->
            ( { c | shiftDown = False }, path, tok )

        Esc ->
            ( c, path, tok )

        Enter ->
            ( c, path, tok )

        Spc ->
            ( c, path, tok )

        Arrow dir ->
            ( c, path, tok )


type KeyboardEvent
    = Char Char
    | Shift
    | Esc
    | Enter
    | Spc
    | Arrow Direction
    | LiftUp


type ModifyTree
    = AddChild Layout Token
    | AddSibiling Token
    | Replace Token
    | Cut


modify : ModifyTree -> TokenPath -> Token -> Token
modify act path tok =
    case ( act, path, tok ) of
        ( AddChild l t, End, Atom k ) ->
            Group k l [ t ]

        ( AddChild _ t, End, Group gt l xs ) ->
            Group gt l <| xs ++ [ t ]

        ( AddChild l t, Next n sub, Atom k ) ->
            Group k l [ t ]

        ( AddChild _ t, Next n sub, Group k l xs ) ->
            updateGroup k l (modify act sub) n xs

        ( AddSibiling t, End, _ ) ->
            treeRoot [ tok, t ]

        ( AddSibiling t, Next n End, Group k l xs ) ->
            ListE.splitAt (n + 1) xs
                |> (\( a, b ) -> a ++ [ t ] ++ b)
                |> Group k l

        ( AddSibiling t, Next n sub, Atom k ) ->
            treeRoot [ tok, t ]

        ( AddSibiling t, Next n sub, Group k l xs ) ->
            updateGroup k l (modify act sub) n xs

        ( Replace t, End, _ ) ->
            t

        ( Replace t, Next n sub, Atom k ) ->
            t

        ( Replace t, Next n sub, Group k l xs ) ->
            updateGroup k l (modify act sub) n xs

        ( Cut, End, _ ) ->
            empty

        ( Cut, Next n End, Atom _ ) ->
            tok

        ( Cut, Next n End, Group k l xs ) ->
            Group k l (ListE.removeAt n xs)

        ( Cut, Next n sub, Atom _ ) ->
            tok

        ( Cut, Next n sub, Group k l xs ) ->
            updateGroup k l (modify act sub) n xs


updateGroup k l fn n xs =
    Group k l <| runAt fn n xs


runAt fn idx xs =
    ListE.updateAt idx fn xs


type MovePath
    = ToFirstChild
    | ToNextSibiling
    | ToParent
    | ToLastChild
    | ToPrevSibiling


movePath : MovePath -> TokenPath -> Token -> Maybe TokenPath
movePath moveCmd path tok =
    case ( moveCmd, path, tok ) of
        ( ToNextSibiling, End, _ ) ->
            Nothing

        ( ToNextSibiling, _, Atom _ ) ->
            Nothing

        ( ToNextSibiling, Next n End, Group k l xs ) ->
            nextSibiling n (selectable xs)

        ( ToNextSibiling, Next n sub, Group k l xs ) ->
            ListE.getAt n xs
                |> Maybe.andThen (movePath moveCmd sub)
                |> Maybe.map (Next n)

        ( ToPrevSibiling, End, _ ) ->
            Nothing

        ( ToPrevSibiling, _, Atom _ ) ->
            Nothing

        ( ToPrevSibiling, Next n End, Group _ _ xs ) ->
            prevSibiling n (selectable xs)

        ( ToPrevSibiling, Next n sub, Group _ _ xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToParent, End, _ ) ->
            Nothing

        ( ToParent, Next _ End, _ ) ->
            Just End

        ( ToParent, Next n sub, Atom _ ) ->
            Nothing

        ( ToParent, Next n sub, Group k l xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToFirstChild, End, Atom _ ) ->
            Nothing

        ( ToFirstChild, Next _ _, Atom _ ) ->
            Nothing

        ( ToFirstChild, End, Group _ _ xs ) ->
            firstChild xs End

        ( ToFirstChild, Next n sub, Group _ _ xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToLastChild, End, Atom _ ) ->
            Nothing

        ( ToLastChild, Next _ _, Atom _ ) ->
            Nothing

        ( ToLastChild, End, Group _ _ xs ) ->
            lastChild xs End

        ( ToLastChild, Next n sub, Group _ _ xs ) ->
            moveInGroup moveCmd n sub xs


moveInGroup moveCmd n sub xs =
    ListE.getAt n xs
        |> Maybe.andThen (movePath moveCmd sub)
        |> Maybe.map (Next n)


type Msg
    = Keyboard KeyboardEvent
    | UserMouseHover Token TokenPath


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
                    [ Browser.Events.onKeyDown (Json.succeed (Keyboard Spc))
                    ]
        }


testState : Token
testState =
    group Vert
        [ group Hori
            [ token (TypeName "State"), syntax ":" ]
        , group Vert
            [ syntax "{"
            , group Hori
                [ token (FieldName "session")
                , syntax " : "
                , token (TypeName "Session")
                ]
            , group Hori
                [ token <| FieldName "problems"
                , syntax " : "
                , tokenWith (TypeName "List") [ token <| TypeName "Problem" ]
                ]
            , group Hori
                [ token (FieldName "form")
                , syntax " : "
                , tokenWith (TypeName "Maybe") [ token (TypeName "Form") ]
                ]
            , syntax "}"
            ]
        ]


testViewFn : Token
testViewFn =
    group Vert
        [ group Hori
            [ token (FnName "view")
            , syntax ":"
            , token (TypeName "State")
            , syntax "->"
            , tokenWith (TypeName "Elem") [ token (TypeName "Event") ]
            ]
        , group Hori
            [ token (FnName "view")
            , syntax "="
            , syntax "\\"
            , token (ParamName "state")
            , syntax "->"
            ]
        , group Vert
            [ group Hori
                [ token (VarName "content")
                , syntax "="
                , tokenWith (FnName "col")
                    [ group Hori [ syntax "{", syntax "}" ]
                    , group Vert
                        [ syntax "["
                        , syntax "]"
                        ]
                    ]
                ]
            , group Hori
                [ syntax "{"
                , group Hori
                    [ token (FieldName "title")
                    , syntax ":"
                    , token (StrVal "Register")
                    , syntax ","
                    ]
                , token (VarName "content")
                , syntax "}"
                ]
            ]
        ]
