module Editor exposing (main)
import Element exposing (minimum)

import Browser
import Element exposing (Element, alignBottom, alignRight, centerX, centerY, column, el, fill, height, none, padding, paddingXY, pointer, px, rgb, rgb255, rgba255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (Font)
import Element.Input exposing (button)
import Element exposing (fillPortion)


type alias Model =
    { project : RocProject
    , openProgram : Bool
    , openModules : Bool
    , windows : List Window
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
    = Record String (List Field)
    | Union String (List Constructor)


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


type Window
    = Browsing BW Module
    | Open BW Module
    | Pinned BW Module
    | Editing BW Module



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
                  , main = m
                  , modules = []
                  }
                ]
            , dependencies =
                [ { name = "Http 2.0", source = "github/roc/curl", modules = [ m, m, m ] }
                , { name = "RUI 42.0", source = "github/roc/rui", modules = [ m, m, m ] }
                ]
            , modules = [ m, m, m ]
            }
      , openModules = True
      , openProgram = True
      , windows =
            [ Browsing { width = 480, height = 800 } m
            , Browsing { width = 480, height = 1800 } m
            , Pinned { width = 230, height = 200 } m
            ]
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
    { title = "RE"
    , body =
        [ Element.layout
            [ height fill
            , width fill
            , Background.gradient
                { angle = 0
                , steps =
                    [ rgb255 0 0 69
                    , rgb255 20 30 239
                    ]
                }
            , Font.family [ Font.typeface "Helvetica Neue", Font.sansSerif ]
            ]
            (editor model)
        ]
    }


editor : Model -> Element Msg
editor model =
    column [ height fill, width fill ]
        [ row [ width fill, height fill ]
            [ if model.openProgram then
                column
                    [ Background.color (rgba255 220 220 245 0.8)
                    , height fill
                    , spacing 16
                    ]
                    [ el [ paddingXY 4 4, Font.bold, Font.size 24 ] (text model.project.name)
                    , column [ width fill ]
                        (List.map
                            (\p ->
                                column [ width fill ]
                                    [ el [] (text "Platform")
                                    , el [] (text p.name)
                                    , viewNavModule p.main
                                    , column [ width fill ] (List.map viewNavModule p.modules)
                                    ]
                            )
                            model.project.platforms
                        )
                    , column [ width fill, spacing 18 ]
                        (el [] (text "Dependencies")
                            :: List.map
                                (\d ->
                                    column [ width fill, spacing 8 ]
                                        [ el [ Font.bold ] (text d.name)
                                        , el [ Font.size 12 ] (text d.source)
                                        , column [ width fill, spacing 4 ] (List.map viewNavModule d.modules)
                                        ]
                                )
                                model.project.dependencies
                        )
                    ]

              else
                none
            , if model.openModules then
                column
                    [ Background.color (rgb255 200 200 245)
                    , height fill
                    
                    , spacing 16
                    ]
                <|
                    el [] (text "Modules:")
                        :: List.map
                            viewNavModule
                            model.project.modules

              else
                none
            ]
        , row
            [ width fill
            , height (px 44)
            , Background.color (rgba255 175 175 175 0.4)
            ]
            [ button
                [ Font.size 14
                ]
                { onPress = Just UserToggleProgram, label = text "Program" }
            , button
                [ Font.size 14
                ]
                { onPress = Just UserToggleModules, label = text "Modules" }
            , button
                [ centerX
                , Font.size 24
                , Font.bold
                ]
                { onPress = Nothing, label = text "Search Anything" }
            ]
        ]


viewNavModule : Module -> Element Msg
viewNavModule mdl =
    column [ width fill, rounded 8, pointer ]
        [ el [ paddingXY 4 8, Background.color (rgb 0.9 0.9 0.9), width fill, Font.family [ Font.monospace ] ] (text mdl.name)
        , row [ width fill, height (px 8), Background.color (rgb 0 1 0) ] []
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 20 230 45)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        , height fill
        ]
        (text "stylish!")


type Msg
    = UserToggleProgram
    | UserToggleModules


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
