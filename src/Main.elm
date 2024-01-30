module Main exposing (..)

import Browser
import Element exposing (Element, alpha, centerX, centerY, column, el, fill, height, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (..)
import Random
import String exposing (fromInt)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Beat
    = Quarter
    | QuarterRest
    | EightRestEightNote
    | DoubleEightNote


type alias Measure =
    { one : Beat
    , two : Beat
    , three : Beat
    , four : Beat
    }


type alias Model =
    { bpm : Float
    , currentBeat : Int
    , currentMeasure : Measure
    , nextMeasure : Measure
    }


beat : Random.Generator Beat
beat =
    Random.uniform Quarter [ QuarterRest, EightRestEightNote, DoubleEightNote ]


measure : Random.Generator Measure
measure =
    Random.map4 Measure beat beat beat beat


type Msg
    = Tick Time.Posix
    | Roll Time.Posix
    | NewMeasure Measure
    | ChangeBpm Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentBeat = 1
      , currentMeasure = Measure Quarter QuarterRest EightRestEightNote DoubleEightNote
      , nextMeasure = Measure Quarter QuarterRest EightRestEightNote DoubleEightNote
      , bpm = 120
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.currentBeat == 4 then
                ( model
                , Random.generate NewMeasure measure
                )

            else
                ( { bpm = model.bpm
                  , currentBeat = model.currentBeat + 1
                  , currentMeasure = model.currentMeasure
                  , nextMeasure = model.nextMeasure
                  }
                , Cmd.none
                )

        Roll _ ->
            ( model
            , Random.generate NewMeasure measure
            )

        NewMeasure newMeasure ->
            ( { currentBeat = 1
              , currentMeasure = model.nextMeasure
              , nextMeasure = newMeasure
              , bpm = model.bpm
              }
            , Cmd.none
            )

        ChangeBpm newBpm ->
            ( { currentBeat = model.currentBeat
              , currentMeasure = model.currentMeasure
              , nextMeasure = model.nextMeasure
              , bpm = newBpm
              }
            , Cmd.none
            )



-- SUB


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (60000 / model.bpm) Tick



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 212 212 212) ]
        (column
            [ centerX, centerY, spacing 30 ]
            [ Input.slider
                [ height (px 30)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (el
                        [ width fill
                        , height (px 2)
                        , centerY
                        , Background.color (rgb255 255 255 255)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = ChangeBpm
                , label =
                    Input.labelAbove []
                        (text (fromInt (round model.bpm)))
                , min = 40
                , max = 200
                , step = Nothing
                , value = model.bpm
                , thumb =
                    Input.defaultThumb
                }
            , currentMeasure model
            , nextMeasure model
            ]
        )


currentMeasure : Model -> Element msg
currentMeasure model =
    row [ width fill, centerY, spacing 30 ]
        [ beatTile model.currentMeasure.one (model.currentBeat == 1)
        , beatTile model.currentMeasure.two (model.currentBeat == 2)
        , beatTile model.currentMeasure.three (model.currentBeat == 3)
        , beatTile model.currentMeasure.four (model.currentBeat == 4)
        ]


nextMeasure : Model -> Element msg
nextMeasure model =
    row [ width fill, centerY, spacing 30, alpha 0.5 ]
        [ beatTile model.nextMeasure.one False
        , beatTile model.nextMeasure.two False
        , beatTile model.nextMeasure.three False
        , beatTile model.nextMeasure.four False
        ]


beatTile : Beat -> Bool -> Element msg
beatTile note highlight =
    el
        [ centerX
        , if highlight then
            Background.color (rgb255 200 255 200)

          else
            Background.color (rgb255 255 255 255)
        , Font.family
            [ Font.typeface "Musisync"
            , Font.sansSerif
            ]
        , Font.size 200
        , Border.rounded 12
        , padding 30
        , width (px 300)
        ]
        (case note of
            Quarter ->
                text "q"

            QuarterRest ->
                text "Q"

            EightRestEightNote ->
                text "Ee"

            DoubleEightNote ->
                text "n"
        )
