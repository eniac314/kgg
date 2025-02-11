module Style.Helpers exposing
    ( buttonStyle
    , buttonStyle_
    , chromeHeightFix
    , contentPadding
    , isMobile
    , isPhone
    , isTablet
    , linkStyle
    , noAttr
    , noHtmlAttr
    , okMark
    , parseCss
    , progressBar
    , sides
    , textInputStyle
    , unselectable
    )

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Element.Region as Region
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Json.Decode as D
import Set exposing (..)
import String.Extra exposing (leftOfBack)
import Style.Palette exposing (..)


chromeHeightFix =
    --supposed to be added to containers with a scrollbar
    htmlAttribute <| HtmlAttr.style "min-height" "auto"


isMobile model =
    isPhone model || isTablet model


isPhone model =
    case .class (classifyDevice model) of
        Phone ->
            True

        _ ->
            False


isTablet model =
    case .class (classifyDevice model) of
        Tablet ->
            True

        _ ->
            False


linkStyle =
    [ pointer, Font.color lightBlue, Font.underline ]


contentPadding config =
    case config.device.class of
        Phone ->
            4

        _ ->
            15


mainMenuHeight model =
    case model.device.class of
        Phone ->
            55

        Tablet ->
            65

        Desktop ->
            75

        BigDesktop ->
            75


footerHeight model =
    case model.device.class of
        Phone ->
            30

        Tablet ->
            40

        Desktop ->
            40

        BigDesktop ->
            40


navHeight model =
    if model.device.class == Phone then
        35

    else
        50


unselectable =
    List.map htmlAttribute
        [ HtmlAttr.style "-webkit-touch-callout" "none"
        , HtmlAttr.style "-webkit-user-select" "none"
        , HtmlAttr.style "-khtml-user-select" "none"
        , HtmlAttr.style "-moz-user-select" "none"
        , HtmlAttr.style "-ms-user-select" "none"
        , HtmlAttr.style "user-select" "none"
        ]


undraggable =
    List.map htmlAttribute
        [ HtmlAttr.style "-webkit-touch-callout" "none"
        , HtmlAttr.style "-webkit-user-drag" "none"
        , HtmlAttr.style "-khtml-user-select" "none"
        , HtmlAttr.style "-moz-user-select" "none"
        , HtmlAttr.style "-ms-user-select" "none"
        , HtmlAttr.style "user-drag" "none"
        ]


horizontalFlip =
    List.map htmlAttribute
        [ HtmlAttr.style "-moz-transform" "scale(-1, 1)"
        , HtmlAttr.style "-webkit-transform" "scale(-1, 1)"
        , HtmlAttr.style "-o-transform" "scale(-1, 1)"
        , HtmlAttr.style "-ms-transform" "scale(-1, 1)"
        , HtmlAttr.style "transform:" "scale(-1, 1)"
        ]


sides =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


noAttr =
    htmlAttribute <| HtmlAttr.class ""


noHtmlAttr =
    HtmlAttr.class ""


buttonStyle isActive =
    [ centerX
    , padding 10
    , Background.color white
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    , Border.width 1
    , Border.rounded 2
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ mouseOver
                    [ Background.color grey
                    ]
                ]

            else
                [ alpha 0.3
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                ]
           )


buttonStyle_ isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ Background.color (rgb 0.9 0.9 0.9)
                , mouseOver [ Font.color (rgb 255 255 255) ]
                , Border.width 1
                , Border.color (rgb 0.9 0.9 0.9)
                ]

            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )


textInputStyle =
    [ width (px 250)
    , paddingXY 5 5
    , spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]


okMark =
    el
        [ Font.bold
        , Font.color (rgb255 92 184 92)
        ]
        (text "✓")


progressBar : Int -> Element msg
progressBar n =
    row
        [ width (px 200)
        , height (px 25)
        , Border.innerShadow
            { offset = ( 0, 1 )
            , size = 1
            , blur = 1
            , color = rgb255 127 127 127
            }
        , Background.color (rgb255 245 245 245)
        , Border.rounded 5
        , clip
        , inFront <|
            el
                [ width (px 200)
                , height (px 25)
                , Font.center
                ]
                (el
                    [ centerX
                    , centerY
                    ]
                    (String.fromInt n
                        |> String.padLeft 2 '0'
                        |> strCons "%"
                        |> text
                    )
                )
        ]
        [ el
            [ width (fillPortion n)
            , height fill
            , Background.color
                (if n < 25 then
                    rgb255 217 83 79

                 else if n < 50 then
                    rgb255 240 173 78

                 else if n < 75 then
                    rgb255 91 192 222

                 else
                    rgb255 92 184 92
                )
            , Font.center
            ]
            Element.none
        , el
            [ width (fillPortion (100 - n))
            , height fill
            ]
            Element.none
        ]


strCons : String -> String -> String
strCons tail head =
    head ++ tail


parseCss s =
    { attribute = String.Extra.leftOf ":" s, value = String.Extra.rightOf ":" s }



-------------------------------------------------------------------------------
