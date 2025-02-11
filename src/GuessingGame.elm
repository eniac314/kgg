module GuessingGame exposing (..)

--import Pages.Auth exposing (getUserName)

import Browser.Dom as Dom
import Codec exposing (..)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.HexColor exposing (rgbCSSHex)
import Element.Input as Input
import Html
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E
import Keyboard as K
import Lamdera exposing (sendToBackend)
import List.Extra
import String.Extra
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Svg
import Svg.Attributes as Svga
import Task
import Time
import Types exposing (..)


toCmd c =
    Task.perform (\_ -> c) (Task.succeed "")


mbStr s =
    if s == "" then
        Nothing

    else
        Just s


hostGame model =
    case model.thisPlayer of
        Just player ->
            ( model
            , sendToBackendWithTime <| \t -> CreateGameTB player { kanjiSet = JlptSet [ 5 ], roundLength = 20, startingCountdown = 120 } t
            )

        _ ->
            ( model, Cmd.none )


joinGame model gameId =
    case model.thisPlayer of
        Just player ->
            ( model
            , sendToBackend <| JoinTB player gameId
            )

        _ ->
            ( model, Cmd.none )


leaveGame model gameId =
    case model.thisPlayer of
        Just player ->
            ( model
            , sendToBackend <| LeaveTB player gameId
            )

        _ ->
            ( model, Cmd.none )


startGame model gameId =
    case ( model.thisPlayer, get gameId model.kggames ) of
        ( Just p, Just { host } ) ->
            if p == host then
                ( model
                , sendToBackendWithTime (\t -> StartTB gameId t)
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


sendToBackendWithTime : (Time.Posix -> ToBackend) -> Cmd FrontendMsg
sendToBackendWithTime callback =
    Task.perform (\time -> SendToBackendWithTime (callback time)) Time.now


wordInput model word =
    ( { model | kggWordInput = mbStr word, kggWrongWordBuffer = Nothing }, Cmd.none )


sendWord model gameId =
    case ( model.thisPlayer, model.kggWordInput ) of
        ( Just p, Just word ) ->
            ( model
            , sendToBackend <| AddWordTB gameId p word
            )

        _ ->
            ( model, Cmd.none )


requestNextKanji model gameId =
    case model.thisPlayer of
        Just p ->
            ( model
            , sendToBackend <| RequestNextKanjiTB gameId p
            )

        _ ->
            ( model, Cmd.none )


setKanjiSet : FrontendModel -> KanjiSet -> Int -> ( FrontendModel, Cmd FrontendMsg )
setKanjiSet model kanjiSet gameId =
    updateGame model
        gameId
        (\g ->
            case g.gameState of
                Lobby config ->
                    let
                        newConfig =
                            { config | kanjiSet = kanjiSet }
                    in
                    ( { g | gameState = Lobby newConfig }, sendToBackend <| UpdateConfigTB gameId newConfig )

                _ ->
                    ( g, Cmd.none )
        )


setCustomKanjiSet : FrontendModel -> String -> ( FrontendModel, Cmd FrontendMsg )
setCustomKanjiSet model kanjiStr =
    ( { model | kggConfigInputs = (\ci -> { ci | kggRandomKanjiInput = mbStr kanjiStr }) model.kggConfigInputs }
    , Cmd.none
    )


updateGame model gameId setter =
    case Dict.get gameId model.kggames of
        Just g ->
            let
                ( newGame, newCmd ) =
                    setter g
            in
            ( { model | kggames = Dict.insert gameId newGame model.kggames }, newCmd )

        _ ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
-- View


view : FrontendModel -> Element FrontendMsg
view model =
    let
        mbHostGame =
            case model.thisPlayer of
                Just p ->
                    Just <| KggHostGame

                Nothing ->
                    Nothing
    in
    column
        [ padding 15, spacing 20 ]
        [ el [ Font.bold, Font.size 25 ] (text "Mini jeu Kanji")
        , if not (isHostingGame model) && not (isCurrentlyPlaying model) then
            Input.button (buttonStyle_ (mbHostGame /= Nothing))
                { onPress = mbHostGame
                , label = text "Héberger une nouvelle partie"
                }

          else
            Element.none
        , column
            [ spacing 30 ]
            (List.map (\g -> gameView g model.kggConfigInputs model.thisPlayer model.kggWordInput model.kggWrongWordBuffer) (Dict.values model.kggames))
        ]


isHostingGame : FrontendModel -> Bool
isHostingGame model =
    case model.thisPlayer of
        Just p ->
            List.member p (List.map .host (Dict.values model.kggames))

        Nothing ->
            False


isCurrentlyPlaying : FrontendModel -> Bool
isCurrentlyPlaying model =
    case model.thisPlayer of
        Just p ->
            List.member p
                (List.concatMap
                    (\g ->
                        case g.gameState of
                            InPlay _ ->
                                g.players

                            _ ->
                                []
                    )
                    (Dict.values model.kggames)
                )

        Nothing ->
            False


gameView :
    KanjiGuessingGame
    ->
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    -> Maybe Player
    -> Maybe String
    -> Maybe String
    -> Element FrontendMsg
gameView game configInputs mbThisPlayer buffer wrongWord =
    let
        isHost =
            Just game.host == mbThisPlayer

        hasJoined =
            isHost || (Maybe.map (\p -> List.member p game.players) mbThisPlayer |> Maybe.withDefault False)
    in
    case mbThisPlayer of
        Just thisPlayer ->
            column
                [ padding 15
                , spacing 15
                ]
                [ --row [] [ text "game id: ", text <| String.fromInt game.gameId ]
                  --, row [] [ text "host: ", playerView game.players game.host ]
                  --, column [ spacing 10 ] (List.map (playerView game.players) game.players)
                  gameStateView game.gameState configInputs game.players thisPlayer game.gameId isHost hasJoined buffer wrongWord game.buffering
                ]

        Nothing ->
            el [ padding 15 ] (text "Chargement en cours...")


playerView : List Player -> Player -> Element msg
playerView ps p =
    let
        color =
            playerColors ps |> Dict.get p.name |> Maybe.withDefault black
    in
    el
        [ paddingXY 10 7
        , Font.size 16
        , Font.color white
        , Background.color color
        , Border.rounded 5
        ]
        (text p.name)


gameStateView :
    KGGameState
    ->
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    -> List Player
    -> Player
    -> Int
    -> Bool
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Bool
    -> Element FrontendMsg
gameStateView gameState configInputs players thisPlayer gameId isHost hasJoined buffer wrongWord buffering =
    case gameState of
        Lobby config ->
            lobbyView config configInputs gameId players thisPlayer isHost hasJoined buffering

        InPlay substate ->
            inPlayView gameId players thisPlayer substate buffer wrongWord buffering

        GameOver substate ->
            endedView substate

        Victory substate ->
            text "Victory!"


lobbyView :
    { kanjiSet : KanjiSet, startingCountdown : Int, roundLength : Int }
    ->
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    -> GameId
    -> List Player
    -> Player
    -> Bool
    -> Bool
    -> Bool
    -> Element FrontendMsg
lobbyView config configInputs gameId players thisPlayer isHost hasJoined buffering =
    column
        [ padding 15
        , spacing 15
        , centerX
        , width (px 400)
        ]
        [ el [ centerX, Font.size 22, Font.semiBold ] (text "Nouveau jeu - Configuration")
        , paragraph [ Font.center, width fill ] (List.map (el [ paddingXY 7 0 ] << playerView players) players)
        , el [ centerX ] <|
            if isHost then
                column
                    [ spacing 20, centerX ]
                    [ column
                        [ spacing 15 ]
                        [ row [ spacing 15 ]
                            [ Input.checkbox []
                                { onChange = always <| KggSetKanjiSet (updateKanjiSetJlpt config.kanjiSet 5) gameId
                                , icon = Input.defaultCheckbox
                                , checked = kanjiSetHasJlptLevel config.kanjiSet 5
                                , label =
                                    Input.labelRight []
                                        (text "N5")
                                }
                            , Input.checkbox []
                                { onChange = always <| KggSetKanjiSet (updateKanjiSetJlpt config.kanjiSet 4) gameId
                                , icon = Input.defaultCheckbox
                                , checked = kanjiSetHasJlptLevel config.kanjiSet 4
                                , label =
                                    Input.labelRight []
                                        (text "N4")
                                }
                            , Input.checkbox []
                                { onChange = always <| KggSetKanjiSet (updateKanjiSetJlpt config.kanjiSet 3) gameId
                                , icon = Input.defaultCheckbox
                                , checked = kanjiSetHasJlptLevel config.kanjiSet 3
                                , label =
                                    Input.labelRight []
                                        (text "N3")
                                }
                            , Input.checkbox []
                                { onChange = always <| KggSetKanjiSet (updateKanjiSetJlpt config.kanjiSet 2) gameId
                                , icon = Input.defaultCheckbox
                                , checked = kanjiSetHasJlptLevel config.kanjiSet 2
                                , label =
                                    Input.labelRight []
                                        (text "N2")
                                }
                            ]
                        ]
                    , el [ centerX ] <|
                        Input.text textInputStyle
                            { onChange = KggSetCustomKanjiSet
                            , text = configInputs.kggRandomKanjiInput |> Maybe.withDefault ""
                            , placeholder = Nothing
                            , label = Input.labelAbove [] (text "Custom kanji set")
                            }
                    , el [ centerX ] <|
                        Input.button (buttonStyle_ True)
                            { onPress = Just (KggStartGame gameId)
                            , label = text "Commencer"
                            }
                    ]

            else if not hasJoined then
                column [ spacing 20 ]
                    [ configView config configInputs
                    , Input.button (buttonStyle_ True)
                        { onPress = Just (KggJoinGame gameId)
                        , label = text "Rejoindre"
                        }
                    ]

            else
                Input.button (buttonStyle_ True)
                    { onPress = Just (KggLeaveGame gameId)
                    , label = text "Quitter"
                    }
        ]


configView config configInputs =
    column
        [ spacing 15 ]
        [ case config.kanjiSet of
            JlptSet set ->
                row
                    []
                    [ el [ Font.semiBold ] (text "Niveau JLPT: ")
                    , el [ Font.size 16 ] (List.map String.fromInt set |> String.join ", " |> text)
                    ]

            CustomKanjiSet kanji ->
                Element.none
        ]


kanjiSetHasJlptLevel kanjiset n =
    case kanjiset of
        JlptSet xs ->
            List.member n xs

        _ ->
            False


updateKanjiSetJlpt kanjiset n =
    case kanjiset of
        JlptSet xs ->
            JlptSet <|
                if List.member n xs then
                    List.Extra.remove n xs

                else
                    n :: xs

        _ ->
            kanjiset


inPlayView gameId players thisPlayer substate buffer wrongWord buffering =
    column
        [ padding 15
        , spacing 15
        , centerX
        , width (px 400)
        , if buffering then
            inFront <| el [ alignRight, Font.size 14 ] (text "buffering!")

          else
            noAttr
        ]
        [ el [ centerX, Font.size 22, Font.semiBold ] (text "Partie en cours")
        , paragraph [ Font.center, width fill ] (List.map (el [ paddingXY 7 0 ] << playerView players) players)
        , row [ centerX ]
            [ el [ Font.size 18, Font.semiBold ] <| text "Score: "
            , el [ Font.size 16 ] <| text (String.fromInt substate.score)
            ]
        , mainKanjiView substate.roundLength substate.timeTillRoundEnd substate.currentKanji
        , countDownView substate.startingCountdown substate.timeTillGameOver

        --, el
        --    [ centerX
        --    , Border.color lightBlue
        --    , Border.width 1
        --    , Border.rounded 5
        --    , Font.size 85
        --    , padding 15
        --    ]
        --  <|
        --    el [ moveDown 5 ] (text (String.fromChar substate.currentKanji))
        --, row [] [ text "remainingKanji: ", paragraph [] <| List.map (\k -> text (String.fromChar k)) substate.remainingKanji ]
        --, row [] [ text "bufferedKanji: ", paragraph [] <| List.map (\k -> text (String.fromChar k)) substate.bufferedKanji ]
        --, row [] [ text "last updated: ", text <| String.fromInt game.lastUpdated ]
        --, row [] [ text "roundLength: ", text <| String.fromInt substate.timeTillRoundEnd ]
        , addWordView gameId thisPlayer buffer wrongWord
        , paragraph
            [ Font.center
            , width fill
            ]
            (Dict.toList substate.words
                |> List.map
                    (\( pId, v ) ->
                        paragraph [ spacing 15 ] <|
                            List.map (el [ paddingXY 5 10, htmlAttribute <| HtmlAttr.attribute "white-space" "nowrap" ] << wordView players pId) v
                    )
            )
        ]


countDownView startingCountdown timeTillGameOver =
    let
        length =
            if timeTillGameOver > startingCountdown then
                300

            else
                round <| 300 * toFloat timeTillGameOver / toFloat startingCountdown
    in
    el
        [ height (px 4)
        , width (px length)
        , Background.color lightBlue
        , centerX
        , Border.rounded 2
        ]
        Element.none


mainKanjiView roundLength timeTillRoundEnd kanji =
    let
        remainingTimeAngle =
            --if roundLength == timeTillRoundEnd then
            --    -pi / 2 + 2 * pi
            --else
            -pi / 2 + 2 * pi * (toFloat (timeTillRoundEnd - roundLength) / toFloat roundLength)

        endX =
            String.fromInt <| round <| 700 + 600 * cos remainingTimeAngle

        endY =
            String.fromInt <| round <| 700 + 600 * sin remainingTimeAngle

        largeArcFlag =
            --String.fromInt <| 1
            String.fromInt <|
                if remainingTimeAngle < -pi / 2 && remainingTimeAngle > -3 * pi / 2 then
                    1

                else
                    0
    in
    el
        [ centerX
        , Border.color lightBlue
        , width (px 140)
        , height (px 140)
        , Svg.svg
            [ Svga.viewBox "0 0 1400 1400"
            ]
            [ Svg.rect
                [ Svga.width "100%"
                , Svga.height "100%"

                --, Svga.fill "rgba(255, 216, 230, 0.5)"
                , Svga.fill "none"
                ]
                []
            , Svg.circle
                [ Svga.cx "700"
                , Svga.cy "700"
                , Svga.r "600"
                , Svga.fill "none"
                , Svga.stroke "black"
                , Svga.strokeWidth "1"
                ]
                []
            , if roundLength == timeTillRoundEnd then
                Svg.circle
                    [ Svga.cx "700"
                    , Svga.cy "700"
                    , Svga.r "600"
                    , Svga.fill "none"
                    , Svga.stroke "lightGreen"
                    , Svga.strokeWidth "70"
                    ]
                    []

              else
                Svg.path
                    [ Svga.d <| "M700,100 A600,600 0 " ++ largeArcFlag ++ ",1 " ++ endX ++ "," ++ endY
                    , Svga.fill "none"
                    , Svga.stroke "lightGreen"
                    , Svga.strokeWidth "70"
                    ]
                    []
            ]
            |> html
            |> el [ width (px 140), height (px 140) ]
            |> behindContent
        ]
    <|
        el [ moveDown 35, moveRight 28, Font.size 85 ] (text (String.fromChar kanji))


wordView : List Player -> String -> String -> Element FrontendMsg
wordView ps pId w =
    case List.filter (\{ id } -> id == pId) ps of
        p :: [] ->
            let
                color =
                    playerColors ps |> Dict.get p.name |> Maybe.withDefault black
            in
            el
                [ paddingXY 10 7
                , Font.size 16
                , Font.color white
                , Background.color color
                , Border.rounded 5
                ]
                (text w)

        _ ->
            Element.none


addWordView : Int -> Player -> Maybe String -> Maybe String -> Element FrontendMsg
addWordView gameId thisPlayer buffer wrongWord =
    column
        [ spacing 15
        , if wrongWord /= Nothing then
            Font.color red

          else
            noAttr
        , centerX
        , paddingEach { sides | top = 5 }
        ]
        [ Input.text (textInputStyle ++ [ htmlAttribute <| HtmlAttr.id "promptingInput" ])
            { onChange = KggWordInput
            , text = buffer |> Maybe.withDefault ""
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , row
            [ spacing 15
            , Font.size 16
            , centerX
            ]
            [ Input.button
                (buttonStyle_ True)
                { onPress = Just (KggRequestNextKanji gameId)
                , label = text "Suivant"
                }
            , Input.button
                (buttonStyle_ (buffer /= Nothing))
                { onPress = Maybe.map (\w -> KggSendWord gameId) buffer
                , label = text "Envoyer"
                }
            ]
        ]


endedView substate =
    column
        [ padding 15
        , spacing 15
        ]
        [ text "GAMEOVER"
        , row [] [ text "score: ", text (String.fromInt substate.score) ]
        ]



-------------------------------------------------------------------------------


keyboardMsg model keyMsg =
    Cmd.none



--let
--    pressedKeys =
--        K.update keyMsg []
--in
--( model
--, if List.member K.Enter pressedKeys then
--    toCmd <| KggSendWord 0
--  else
--    Cmd.none
--)
-------------------------------------------------------------------------------


whiteTextColors =
    List.reverse <|
        List.map rgbCSSHex
            [ "#745399"
            , "#250d00"
            , "#bb5548"
            , "#74325c"
            , "#007b43"
            , "#302833"
            , "#1e50a2"
            , "#7a4171"
            , "#006e54"
            , "#895b8a"
            , "#8d6449"
            , "#6e7955"
            , "#43676b"
            , "#5a544b"
            , "#2c4f54"
            , "#44617b"
            , "#00552e"
            , "#007bbb"
            , "#d9333f"
            , "#3e62ad"
            , "#a22041"
            , "#192f60"
            , "#316745"
            , "#884898"
            , "#4f455c"
            , "#aa4c8f"
            , "#b55233"
            , "#726d40"
            , "#165e83"
            , "#4d5aaf"
            , "#65318e"
            , "#250d00"
            , "#bb5548"
            , "#74325c"
            , "#007b43"
            , "#302833"
            , "#1e50a2"
            , "#7a4171"
            , "#006e54"
            , "#895b8a"
            , "#8d6449"
            , "#6e7955"
            , "#43676b"
            , "#5a544b"
            , "#2c4f54"
            , "#44617b"
            , "#00552e"
            , "#007bbb"
            , "#d9333f"
            , "#3e62ad"
            , "#a22041"
            , "#192f60"
            , "#316745"
            , "#884898"
            , "#4f455c"
            , "#aa4c8f"
            , "#b55233"
            , "#726d40"
            , "#165e83"
            , "#4d5aaf"
            , "#65318e"
            ]


playerColors p =
    List.map .name p
        |> List.Extra.unique
        |> (\stds -> List.Extra.zip stds whiteTextColors)
        |> Dict.fromList
