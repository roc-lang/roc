module Editor exposing (main)

import Browser
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Comments as EComments
import Elm.Syntax.Declaration as EDeclaration
import Elm.Syntax.Documentation as EDocumentation
import Elm.Syntax.Exposing as EExposing
import Elm.Syntax.Expression as EExpression
import Elm.Syntax.File as EFile
import Elm.Syntax.Import as EImport
import Elm.Syntax.Infix as EInfix
import Elm.Syntax.Module as EModule
import Elm.Syntax.ModuleName as EModuleName
import Elm.Syntax.Node as ENode
import Elm.Syntax.Pattern as EPattern
import Elm.Syntax.Range as ERange
import Elm.Syntax.Signature as ESignature
import Elm.Syntax.Type as EType
import Elm.Syntax.TypeAlias as ETypeAlias
import Elm.Syntax.TypeAnnotation as ETypeAnnotation
import Html as H exposing (Attribute, Html, text)
import Html.Attributes as A
import Html.Events as Ev


type alias Model =
    { project : RocProject
    , openProgram : Bool
    , openModules : Bool
    , window : Window
    }


type alias RocProject =
    { name : String
    , platforms : List Platform
    , dependencies : List Dependency
    , modules : List Module
    }


type alias Platform =
    { name : String
    , main : Module
    , modules : List Module
    }


type alias Dependency =
    { name : String
    , source : String
    , modules : List Module
    }


type alias Module =
    { name : String
    , imports : List Import
    , types : List Type
    , functions : List Function
    , docs : String
    , tests : List ModuleTest
    }


type alias Import =
    { moduleName : String
    , asName : Maybe String
    , expose : List String
    }


type Type
    = Record String (List Field) Open
    | Union String (List Constructor) Open


type Open
    = Open
    | Sealed


type alias Field =
    { name : String
    , fieldType : Type
    }


type alias Constructor =
    { tag : String
    , arg : List String
    }


type alias Function =
    { name : String
    , args : List ( Type, String )
    , statement : Expression
    }


type Expression
    = IfElse Expression Expression Expression
    | FnCall String (List Expression)
    | Ref String
    | LitNum Int
    | Str String
    | When Expression (List ( List String, Expression ))
    | Assignment String Expression Expression


type alias ModuleTest =
    { name : String
    , test : Expression
    , result : TestResult
    }


type TestResult
    = Ok
    | Err String


type alias Windows =
    { pinned : List WindowModule
    , viewing : List WindowModule
    }



{- there should be more options just these are basic ones , Seleciting and moving, Searching -}


type Window
    = Browsing Windows
    | Expressing String WindowModule Cursor Hints Windows


type alias Hints =
    List Hint


type Hint
    = Hint String -- meh
    | SelectedHint String


type Cursor
    = Idx Int Cursor
    | This


type alias Suggestion =
    { name : String
    , moduleName : String
    }


type alias WindowModule =
    { size : BW
    , orig : Module
    , now : Module
    }



-- Window Basic


type alias BW =
    { width : Int
    , height : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { project =
            { name = "RealWorld"
            , platforms =
                [ { name = "Au Joyeux"
                  , main = { m | name = "Main" }
                  , modules =
                        [ { m | name = "Int" }
                        , { m | name = "Float" }
                        , { m | name = "Str" }
                        , { m | name = "List" }
                        , { m | name = "Dict" }
                        , { m | name = "3dRender" }
                        ]
                  }
                ]
            , dependencies =
                []
            , modules =
                []
            }
      , openModules = True
      , openProgram = True
      , window =
            Expressing "str"
                { orig = m
                , now = m
                , size = { width = 500, height = 800 }
                }
                This
                []
                { pinned =
                    []
                , viewing =
                    [ { orig = m, now = m, size = { width = 800, height = 200 } } ]
                }
      }
    , Cmd.none
    )


m : Module
m =
    { name = "Module"
    , imports = []
    , types = []
    , functions = []
    , docs = "Go Figure"
    , tests = []
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Roc Editor"
    , body = editor model
    }


type alias Node msg =
    String -> El msg


type alias El msg =
    List String -> List (Attribute msg) -> List (Html msg) -> Html msg


node : Node msg
node tag xcs attrs els =
    H.node tag (A.class (String.join " " xcs) :: attrs) els


el : List String -> List (Attribute msg) -> Html msg -> Html msg
el xsc attrs el_ =
    node "div" xsc attrs [ el_ ]


col : El msg
col =
    extend "div" [ "flex", "flex-col" ]


row : El msg
row =
    extend "div" [ "flex", "flex-row" ]


extend : String -> List String -> El msg
extend tag xca xcb =
    node tag (xca ++ xcb)


editor : Model -> List (Html Msg)
editor model =
    let
        toggles p =
            row []
                []
                [ button [] { onPress = Just (ToggleImports p), label = text "Imports" }
                , button [] { onPress = Just (ToggleTypes p), label = text "Types" }
                , button [] { onPress = Just (ToggleFunctions p), label = text "Functions" }
                , button [] { onPress = Just (ToggleDocs p), label = text "Docs" }
                ]

        viewingModules xs =
            node "div"
                [ "grid", "grid-cols-2", "gap-4", "overflow-auto", "flex-1", "h-full" ]
                []
                (xs
                    |> List.map
                        (\p ->
                            node "div"
                                [ "bg-red-400" ]
                                [ A.style "height" (px p.size.height)
                                , A.style "width" (px p.size.width)
                                ]
                                [ node "h2" [] [] [ text p.now.name ]
                                , toggles p
                                , node "import-list"
                                    []
                                    []
                                    (button []
                                        { onPress = Just (PressedAddImport p)
                                        , label = text "+ add import"
                                        }
                                        :: List.map
                                            (\import_ ->
                                                node "module-import"
                                                    []
                                                    []
                                                    [ node "module-name" [] [] [ text import_.moduleName ]
                                                    ]
                                            )
                                            p.now.imports
                                    )
                                , node "types"
                                    []
                                    []
                                    (button []
                                        { onPress = Just (PressedAddImport p)
                                        , label = text "+ add Type"
                                        }
                                        :: List.map viewType p.now.types
                                    )
                                , node "functions"
                                    []
                                    []
                                    (button []
                                        { onPress = Just (PressedAddImport p)
                                        , label = text "+ add Function"
                                        }
                                        :: List.map viewFn p.now.functions
                                    )
                                , node "tests-preview"
                                    [ "bg-red-300", "w-full" ]
                                    []
                                    [ text "No Tests? No Problems"
                                    ]
                                ]
                        )
                )

        pinnedModules xs =
            node "aside"
                [ "flex", "flex-col", "overflow-auto", "h-full", "items-end" ]
                []
                (List.map
                    (\p ->
                        node "div"
                            [ "bg-red-300" ]
                            [ A.style "height" (px p.size.height)
                            , A.style "width" (px p.size.width)
                            ]
                        <|
                            node "h2" [] [] [ text p.orig.name ]
                                :: List.map viewType p.now.types
                                ++ List.map viewFn p.now.functions
                    )
                    xs
                )

        editingModule str edm cur =
            node "editing-module"
                [ "flex", "flex-col", "h-full", "bg-white" ]
                [ A.style "height" (px edm.size.height)
                , A.style "width" (px edm.size.width)
                ]
                [ node "h2" [] [] [ text edm.now.name ]
                , toggles edm
                ]
    in
    [ node "main"
        [ "flex", "flex-row", "overflow-auto", "w-full" ]
        [ A.style "height" "calc(100vh - 40px)" ]
        [ col
            [ "overflow-y-auto"
            , "bg-pink-100"
            , if model.openProgram then
                ""

              else
                "hidden"
            ]
            [ A.id "program" ]
            [ node "h1" [] [] [ H.text model.project.name ]
            , col []
                []
                (List.map
                    (\p ->
                        col []
                            []
                            [ el [] [] (text "Platform")
                            , el [] [] (text p.name)
                            , viewNavModule p.main
                            , el [] [] (text "Built-ins")
                            , col [] [] (List.map viewNavModule p.modules)
                            ]
                    )
                    model.project.platforms
                )
            , col []
                []
                (el [] [] (text "Dependencies")
                    :: List.map
                        (\d ->
                            col []
                                []
                                [ el [] [] (text d.name)
                                , el [] [] (text d.source)
                                , col [] [] (List.map viewNavModule d.modules)
                                ]
                        )
                        model.project.dependencies
                )
            ]
        , col
            [ "overflow-y-auto"
            , "bg-gray-50"
            , if model.openModules then
                ""

              else
                "hidden"
            ]
            []
          <|
            el [] [] (text "Modules:")
                :: button []
                    { onPress = Just PressedAddModule
                    , label = H.text "+ Add Module"
                    }
                :: List.map
                    viewNavModule
                    model.project.modules
        , row [ "flex-1", "height-full", "bg-green-200", "relative", "w-full" ]
            []
          <|
            case model.window of
                Expressing string edModule cursor _ windows ->
                    [ viewingModules windows.viewing
                    , editingModule string edModule cursor
                    , pinnedModules windows.pinned
                    ]

                Browsing b ->
                    [ viewingModules b.viewing
                    , pinnedModules b.pinned
                    ]
        ]
    , node "nav"
        [ "flex", "flex-row", "items-center" ]
        [ A.style "height" "40px" ]
        [ button
            []
            { onPress = Just UserToggleProgram, label = text "Program" }
        , button
            []
            { onPress = Just UserToggleModules, label = text "Modules" }
        , case model.window of
            Browsing _ ->
                button
                    [ "mx-16", "flex-1", "px-4", "bg-gray-400" ]
                    { onPress = Nothing, label = text "Search Anything" }

            Expressing str ewm _ hints _ ->
                node "preview"
                    [ "font-3xl", "font-bold", "font-mono", "mx-16", "px-4", "text-white", "bg-pink-700" ]
                    [ A.style "width" (px ewm.size.width)
                    ]
                    [ text str ]
        ]
    ]


none : Html msg
none =
    text ""


button : List String -> { onPress : Maybe msg, label : Html msg } -> Html msg
button xcs p =
    let
        attrs =
            case p.onPress of
                Just clk ->
                    [ Ev.onClick clk ]

                Nothing ->
                    []
    in
    node "button" xcs attrs [ p.label ]


viewNavModule : Module -> Html Msg
viewNavModule mdl =
    node "button"
        [ "px-2", "py", "mb-2", "bg-gray-50", "cursor-pointer" ]
        [ Ev.onClick (PressedNavModule mdl)
        ]
        [ node "h3" [ "font-mono" ] [] [ H.text mdl.name ]
        , node "tests"
            [ "w-full"
            , "bg-green-500"
            , ""
            , "text-xs"
            ]
            []
            [ text "No Tests, No Problems!" ]
        ]


viewType : Type -> Html Msg
viewType type_ =
    case type_ of
        Record name _ _ ->
            text name

        Union name _ _ ->
            text name


viewFn : Function -> Html Msg
viewFn fun =
    node "function"
        []
        []
        [ node "h2" [] [] [ text fun.name ]
        , node "syntax" [] [] [ text ":" ]
        , node "args" [] [] (List.map viewSmallArgType fun.args)
        ]


viewSmallArgType : ( Type, String ) -> Html Msg
viewSmallArgType ( type_, argName ) =
    case type_ of
        Record s _ _ ->
            node "arg"
                []
                []
                [ node "type" [] [] [ text s ]
                , node "arg-name" [] [] [ text argName ]
                ]

        Union s _ _ ->
            node "arg"
                []
                []
                [ node "type" [] [] [ text s ]
                , node "arg-name" [] [] [ text argName ]
                ]


moduleWindow : BW -> List (Attribute msg) -> List (Html msg) -> Html msg
moduleWindow p atts =
    col [ "bg-yellow-300", "absolute", "rounded-lg", "shadow-lg" ]
        ([ A.style "height" (px p.height)
         , A.style "width" (px p.width)
         ]
            ++ atts
        )


px f =
    String.fromInt f ++ "px"


top x =
    A.style "top" (px x)


type Msg
    = UserToggleProgram
    | UserToggleModules
    | PressedNavModule Module
    | ToggleImports WindowModule
    | ToggleTypes WindowModule
    | ToggleFunctions WindowModule
    | ToggleDocs WindowModule
    | ToggleTests WindowModule
    | PressedAddImport WindowModule
    | PressedAddModule


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedAddModule ->
            let
                prj =
                    model.project

                navModule =
                    { m | name = "New Module " ++ String.fromInt (List.length prj.modules) }
            in
            ( { model
                | window =
                    case model.window of
                        Browsing ws ->
                            Browsing
                                { viewing = newWindow navModule :: ws.viewing
                                , pinned = ws.pinned
                                }

                        Expressing _ _ _ _ wins ->
                            Browsing
                                { viewing = newWindow navModule :: wins.viewing
                                , pinned = wins.pinned
                                }
                , project = { prj | modules = navModule :: prj.modules }
              }
            , Cmd.none
            )

        ToggleImports winModule ->
            ( model, Cmd.none )

        ToggleTypes winModule ->
            ( model, Cmd.none )

        ToggleFunctions winModule ->
            ( model, Cmd.none )

        ToggleDocs winModule ->
            ( model, Cmd.none )

        ToggleTests winModule ->
            ( model, Cmd.none )

        PressedAddImport winModule ->
            ( model, Cmd.none )

        PressedNavModule navModule ->
            ( { model
                | window =
                    case model.window of
                        Browsing ws ->
                            Browsing
                                { viewing = newWindow navModule :: ws.viewing
                                , pinned = ws.pinned
                                }

                        Expressing _ _ _ _ ws ->
                            Browsing
                                { viewing = newWindow navModule :: ws.viewing
                                , pinned = ws.pinned
                                }
              }
            , Cmd.none
            )

        UserToggleProgram ->
            ( { model | openProgram = not model.openProgram }, Cmd.none )

        UserToggleModules ->
            ( { model | openModules = not model.openModules }, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


newWindow fromModule =
    { orig = fromModule
    , now = fromModule
    , size = { width = 420, height = 420 }
    }


ev =
    ENode.value


modName emodule =
    emodule.moduleName
        |> ev
        |> String.join "."


elmToRoc : EFile.File -> Module
elmToRoc efile_ =
    { name =
        case ev efile_.moduleDefinition of
            EModule.NormalModule nm ->
                modName nm

            EModule.PortModule pm ->
                modName pm

            EModule.EffectModule em ->
                modName em
    , imports = []
    , types = []
    , functions = []
    , docs = ""
    , tests = []
    }
