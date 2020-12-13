module Prototype exposing (..)

import Browser
import Browser.Events
import Html as H exposing (Html, p, sub)
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
    , selected : String
    }


type TokenPath
    = End
    | Next Int TokenPath


type Token
    = Token Kind Layout (List Token)


type Kind
    = Empty
    | TypeDeclaration String
    | FnDeclaration String
    | Module String
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
moveStep dir ((Token _ l xs) as tok) path { sibilings, current, layout } =
    case path of
        End ->
            -- root case
            moves tok path sibilings current layout (moveSteps ( dir, layout ))
                |> Maybe.withDefault End

        Next n End ->
            ListE.getAt n xs
                |> Maybe.andThen
                    (\t ->
                        moves t path (selectable xs) n l (moveSteps ( dir, l ))
                    )
                |> Maybe.withDefault End

        Next n sub ->
            ListE.getAt n xs
                |> Maybe.map
                    (\t ->
                        Next n (moveStep dir t sub { sibilings = selectable xs, current = n, layout = l })
                    )
                |> Maybe.withDefault End


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
        Token _ l toks ->
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
getTokenAtPath p ((Token _ l xs) as tok) =
    case p of
        End ->
            tok

        Next n sub ->
            ListE.getAt n xs
                |> Maybe.map (getTokenAtPath sub)
                |> Maybe.withDefault tok


parentOf : TokenPath -> TokenPath
parentOf p =
    case p of
        End ->
            End

        Next n End ->
            End

        Next n sub ->
            Next n (parentOf sub)


selectable : List Token -> List Int
selectable ts =
    ListE.findIndices isSelectable ts


isSelectable : Token -> Bool
isSelectable t =
    case t of
        Token (Syntax _) _ _ ->
            False

        _ ->
            True


token : Kind -> Token
token k =
    Token k Hori []


tokenWith : Kind -> List Token -> Token
tokenWith s xs =
    Token s Hori xs


syntax : String -> Token
syntax s =
    Token (Syntax s) Hori []


group : Layout -> List Token -> Token
group l xs =
    Token Empty l xs


empty : Token
empty =
    token Empty


kind : Token -> Kind
kind (Token k _ _) =
    k


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree =
            treeRoot
                [ token Empty
                ]
      , keyboard = Editing emptyCaret (Next 0 End)
      , shiftDown = False
      , actions = []
      , token = Nothing
      }
    , Cmd.none
    )


emptyCaret : Caret
emptyCaret =
    Caret "" "" False ""


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
                Token (Syntax s) _ [] ->
                    ( H.text s, noStyle, [] )

                Token k _ [] ->
                    case ( mCaret, selPath ) of
                        ( Just car, Just End ) ->
                            ( viewCaret car
                            , A.class "px-2 py-1"
                            , []
                            )

                        _ ->
                            ( H.text "var", tokenStyles (isSel selPath) Hori "var", [] )

                Token name lay xs_ ->
                    ( H.text <| Debug.toString name, tokenStyles (isSel selPath) lay "name", xs_ )

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


caretIsEmpty : Caret -> Bool
caretIsEmpty { before, after, selected } =
    0 == String.length before + String.length after + String.length selected


caretValue : Caret -> String
caretValue { before, after, selected } =
    before ++ selected ++ after


onKeyboardEvent : KeyboardEvent -> Typing -> Typing
onKeyboardEvent ev ( c, path, tree ) =
    let
        cwt =
            getTokenAtPath path tree
    in
    case ev of
        Char char ->
            let
                newChar =
                    if c.shiftDown then
                        Char.toUpper char

                    else
                        char

                typeIn =
                    { c | after = String.append c.after (String.fromChar newChar) }

                ( writeIn, nsp ) =
                    modify (Rename <| caretValue typeIn) path tree

                standard =
                    ( typeIn, nsp, writeIn )
            in
            if closingSyntax newChar then
                let
                    ( newTree, newPath ) =
                        tree
                            |> modify (AddSibiling Hori (syntax (String.fromChar newChar))) path
                            |> Tuple.mapSecond parentOf
                            |> (\( t, p ) -> modify (AddSibiling Vert empty) p t)
                in
                ( emptyCaret
                , newPath
                , newTree
                )

            else if oppeningSyntax newChar then
                Debug.todo "open "

            else if kind cwt == Empty then
                let
                    ( newTree, newPath ) =
                        modify (Replace (token (resolveKind newChar))) path tree
                in
                ( typeIn
                , newPath
                , newTree
                )

            else
                standard

        Shift ->
            ( { c | shiftDown = True }, path, tree )

        LiftUp ->
            ( { c | shiftDown = False }, path, tree )

        Esc ->
            ( c, path, tree )

        Enter ->
            ( c, path, tree )

        Spc ->
            ( c, path, tree )

        Backspace ->
            ( c, path, tree )

        Arrow dir ->
            ( c, path, tree )


closingSyntax : Char -> Bool
closingSyntax c =
    List.member c [ ':', '=', ']', '}', ')', ',' ]


oppeningSyntax : Char -> Bool
oppeningSyntax c =
    List.member c [ '\\', '[', '{', '(', '.' ]


resolveKind : Char -> Kind
resolveKind c =
    if Char.isUpper c then
        TypeName <| String.fromChar c

    else
        FnName <| String.fromChar c


type KeyboardEvent
    = Char Char
    | Shift
    | Esc
    | Enter
    | Spc
    | Backspace
    | Arrow Direction
    | LiftUp


type ModifyTree
    = AddChild Layout Token
    | AddSibiling Layout Token
    | Replace Token
    | Rename String
    | Cut


modify : ModifyTree -> TokenPath -> Token -> ( Token, TokenPath )
modify act path tok =
    case act of
        AddChild cl t ->
            case ( path, tok ) of
                ( End, Token gt l xs ) ->
                    ( Token gt l <| xs ++ [ t ]
                    , Next (List.length xs) End
                    )

                ( Next n sub, Token k _ [] ) ->
                    ( Token k cl [ t ], Next 0 End )

                ( Next n sub, Token k l xs ) ->
                    modifyGroup k l act sub n xs

        AddSibiling sl t ->
            case ( path, tok ) of
                ( End, _ ) ->
                    ( Token Empty sl [ tok, t ], Next 1 End )

                ( Next n End, Token k l xs ) ->
                    ( ListE.splitAt (n + 1) xs
                        |> (\( a, b ) -> a ++ [ t ] ++ b)
                        |> Token k l
                    , Next (n+1) End
                    )

                ( Next n sub, Token k _ [] ) ->
                    ( Token Empty sl [ tok, t ], Next 1 End )

                ( Next n sub, Token k l xs ) ->
                    modifyGroup k l act sub n xs

        Replace t ->
            case ( path, tok ) of
                ( End, _ ) ->
                    ( t, End )

                ( Next n sub, Token k _ [] ) ->
                    ( t, End )

                ( Next n sub, Token k l xs ) ->
                    modifyGroup k l act sub n xs

        Cut ->
            case ( path, tok ) of
                ( End, _ ) ->
                    ( empty, End )

                ( Next n End, Token _ _ [] ) ->
                    ( tok, End )

                ( Next n End, Token k l xs ) ->
                    ( Token k l (ListE.removeAt n xs), End )

                ( Next n sub, Token _ _ [] ) ->
                    ( tok, End )

                ( Next n sub, Token k l xs ) ->
                    modifyGroup k l act sub n xs

        Rename v ->
            case ( path, tok ) of
                ( End, Token k l xs ) ->
                    ( Token (renameKindValue v k) l xs
                    , End
                    )

                ( Next n sub, Token k l xs ) ->
                    modifyGroup k l act sub n xs


renameKindValue : String -> Kind -> Kind
renameKindValue v k =
    case k of
        TypeName _ ->
            TypeName v

        Syntax _ ->
            Syntax v

        TagName _ ->
            TagName v

        FieldName _ ->
            FieldName v

        ParamName _ ->
            ParamName v

        FnName _ ->
            FnName v

        VarName _ ->
            VarName v

        StrVal _ ->
            StrVal v

        NumVal _ ->
            NumVal v

        TypeDeclaration _ ->
            TypeDeclaration v

        FnDeclaration _ ->
            FnDeclaration v

        Module _ ->
            Module v

        Empty ->
            Empty


modifyGroup :
    Kind
    -> Layout
    -> ModifyTree
    -> TokenPath
    -> Int
    -> List Token
    -> ( Token, TokenPath )
modifyGroup k l act sub n xs =
    ListE.getAt n xs
        |> Maybe.map (modify act sub)
        |> Maybe.Extra.unpack (\_ -> ( Token k l xs, End ))
            (Tuple.mapBoth
                (\t -> Token k l <| ListE.setAt n t xs)
                (Next n)
            )


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

        ( ToNextSibiling, Next n End, Token k l xs ) ->
            nextSibiling n (selectable xs)

        ( ToNextSibiling, Next n sub, Token k l xs ) ->
            ListE.getAt n xs
                |> Maybe.andThen (movePath moveCmd sub)
                |> Maybe.map (Next n)

        ( ToPrevSibiling, End, _ ) ->
            Nothing

        ( ToPrevSibiling, Next n End, Token _ _ xs ) ->
            prevSibiling n (selectable xs)

        ( ToPrevSibiling, Next n sub, Token _ _ xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToParent, End, _ ) ->
            Nothing

        ( ToParent, Next _ End, _ ) ->
            Just End

        ( ToParent, Next n sub, Token k l xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToFirstChild, End, Token _ _ xs ) ->
            firstChild xs End

        ( ToFirstChild, Next n sub, Token _ _ xs ) ->
            moveInGroup moveCmd n sub xs

        ( ToLastChild, End, Token _ _ xs ) ->
            lastChild xs End

        ( ToLastChild, Next n sub, Token _ _ xs ) ->
            moveInGroup moveCmd n sub xs


moveInGroup : MovePath -> Int -> TokenPath -> List Token -> Maybe TokenPath
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
