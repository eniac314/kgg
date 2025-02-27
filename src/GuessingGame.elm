module GuessingGame exposing (..)

--import Pages.Auth exposing (getUserName)

import ApiCalls exposing (revEntityDict)
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
import Helpers exposing (..)
import Html
import Html.Attributes as HtmlAttr
import Html.Parser
import Html.Parser.Util
import Json.Decode as D
import Json.Encode as E
import Keyboard as K
import Lamdera exposing (sendToBackend)
import List.Extra
import Set
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
            , sendToBackendWithTime <|
                \t ->
                    CreateGameTB player
                        { kanjiSet = JlptSet [ 5 ]
                        , roundLength = 20
                        , startingCountdown = 120
                        , showingHints = True
                        }
                        t
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


loadInitialData model gameId =
    case ( model.thisPlayer, get gameId model.kggames ) of
        ( Just p, Just { host } ) ->
            if p == host then
                ( model
                , sendToBackendWithTime (\t -> LoadInitialDataTB gameId t)
                )

            else
                ( model, Cmd.none )

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
            updateGame model
                gameId
                (\g ->
                    ( g
                      --{ g | buffering = True }
                    , sendToBackend <| RequestNextKanjiTB gameId p
                    )
                )

        --( model
        --, sendToBackend <| RequestNextKanjiTB gameId p
        --)
        _ ->
            ( model, Cmd.none )


endGame model gameId =
    ( { model | kggames = Dict.remove gameId model.kggames }, Cmd.none )


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


setShowingHints model gameId bool =
    updateGame model
        gameId
        (\g ->
            case g.gameState of
                Lobby config ->
                    let
                        newConfig =
                            { config | showingHints = bool }
                    in
                    ( { g | gameState = Lobby newConfig }, sendToBackend <| UpdateConfigTB gameId newConfig )

                _ ->
                    ( g, Cmd.none )
        )


setCustomKanjiSet : FrontendModel -> GameId -> String -> ( FrontendModel, Cmd FrontendMsg )
setCustomKanjiSet model gameId kanjiStr =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                Lobby config ->
                    let
                        ( newKanjiSet, newCmd ) =
                            String.toList kanjiStr
                                |> List.filter isHanzi
                                |> (\xs ->
                                        if xs == [] then
                                            ( config.kanjiSet, Cmd.none )

                                        else
                                            ( CustomKanjiSet xs
                                            , sendToBackend <|
                                                UpdateConfigTB gameId
                                                    { config | kanjiSet = CustomKanjiSet xs }
                                            )
                                   )

                        newConfig =
                            Lobby { config | kanjiSet = newKanjiSet }

                        newGame =
                            { game | gameState = newConfig }
                    in
                    ( { model
                        | kggConfigInputs =
                            (\ci -> { ci | kggRandomKanjiInput = mbStr kanjiStr }) model.kggConfigInputs
                        , kggames = Dict.insert gameId newGame model.kggames
                      }
                    , newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


gotJMdictSearchResultsEndGame model gameId word res =
    case res of
        Ok jmdictResults ->
            updateGame model
                gameId
                (\g ->
                    case g.gameState of
                        Victory substate ->
                            let
                                newGameState =
                                    { substate | words = Dict.insert word jmdictResults substate.words }
                            in
                            ( { g | gameState = Victory newGameState }, Cmd.none )

                        GameOver substate ->
                            let
                                newGameState =
                                    { substate | words = Dict.insert word jmdictResults substate.words }
                            in
                            ( { g | gameState = GameOver newGameState }, Cmd.none )

                        _ ->
                            ( g, Cmd.none )
                )

        _ ->
            ( model, Cmd.none )


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
        [ column [ spacing 5, centerX ]
            [ el [ Font.bold, Font.size 25, centerX ] (text "Mini jeu Kanji")
            , el [ Font.size 14, centerX ] (text <| "Jeux en cours : " ++ String.fromInt (Dict.size model.kggames))
            ]
        , if not (isHostingGame model) && not (isCurrentlyPlaying model) then
            el [ centerX ] <|
                Input.button (buttonStyle_ (mbHostGame /= Nothing))
                    { onPress = mbHostGame
                    , label = text "Héberger une nouvelle partie"
                    }

          else
            Element.none
        , case getCurrentGame model of
            Just cg ->
                gameView cg model.kanjidic model.kggConfigInputs model.thisPlayer model.kggWordInput model.kggWrongWordBuffer

            Nothing ->
                column
                    [ spacing 30 ]
                    (List.map (\g -> gameView g model.kanjidic model.kggConfigInputs model.thisPlayer model.kggWordInput model.kggWrongWordBuffer)
                        (Dict.values model.kggames |> List.reverse)
                    )
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


currentGameInitialLoadingStatus gs =
    round <| 100 * (toFloat <| Set.size gs.initialBuffer.done) / toFloat gs.initialBuffer.bufferSize


getCurrentGame : FrontendModel -> Maybe KanjiGuessingGame
getCurrentGame model =
    case model.thisPlayer of
        Just p ->
            List.filter
                (\g ->
                    case g.gameState of
                        InPlay _ ->
                            List.member p g.players

                        _ ->
                            False
                )
                (Dict.values model.kggames)
                |> List.head

        Nothing ->
            Nothing


gameView :
    KanjiGuessingGame
    -> Dict Char KanjidicEntry
    ->
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    -> Maybe Player
    -> Maybe String
    -> Maybe String
    -> Element FrontendMsg
gameView game kanjidic configInputs mbThisPlayer buffer wrongWord =
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
                [ gameStateView game.gameState kanjidic configInputs game.players thisPlayer game.gameId isHost hasJoined buffer wrongWord game.buffering
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
    -> Dict Char KanjidicEntry
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
gameStateView gameState kanjidic configInputs players thisPlayer gameId isHost hasJoined buffer wrongWord buffering =
    case gameState of
        Lobby config ->
            lobbyView config configInputs gameId players thisPlayer isHost hasJoined buffering

        Loading substate ->
            loadingView substate

        InPlay substate ->
            inPlayView kanjidic gameId players thisPlayer substate buffer wrongWord buffering

        GameOver substate ->
            endedView gameId players thisPlayer substate

        Victory substate ->
            text "Victory!"


lobbyView :
    GameConfig
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
                            { onChange = KggSetCustomKanjiSet gameId
                            , text = configInputs.kggRandomKanjiInput |> Maybe.withDefault ""
                            , placeholder = Nothing
                            , label = Input.labelAbove [ centerX ] (text "Liste personalisée")
                            }
                    , Input.checkbox []
                        { onChange = KggSetShowingHints gameId
                        , icon = Input.defaultCheckbox
                        , checked = config.showingHints
                        , label =
                            Input.labelRight []
                                (text "Afficher sens et lectures")
                        }
                    , row [ spacing 15, centerX ]
                        [ el [ centerX ] <|
                            Input.button (buttonStyle_ True)
                                { onPress = Just (KggLoadInitialData gameId)
                                , label = text "Commencer"
                                }
                        , el [ centerX ] <|
                            Input.button (buttonStyle_ True)
                                { onPress = Just (KggLeaveGame gameId)
                                , label = text "Annuler"
                                }
                        ]
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


loadingView substate =
    column
        [ padding 15
        , spacing 10
        , centerX
        , width (px 400)
        ]
        [ el [ centerX ] (text <| "Chargement " ++ String.fromInt (currentGameInitialLoadingStatus substate) ++ "%")
        , image [ centerX, width (px 100) ] { src = "bar.svg", description = "buffering!" }
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


inPlayView kanjidic gameId players thisPlayer substate buffer wrongWord buffering =
    let
        thisPlayerIsPlaying =
            List.member thisPlayer players

        mbKanjiMeta =
            Dict.get substate.currentKanji kanjidic
    in
    column
        [ padding 15
        , spacing 15
        , centerX
        , width (px 400)
        ]
        [ el
            [ centerX
            , Font.size 22
            , Font.semiBold
            , paddingXY 60 0
            , if buffering && thisPlayerIsPlaying then
                inFront <| image [ alignLeft, height (px 25), width (px 25) ] { src = "loading2.svg", description = "buffering!" }

              else
                noAttr
            , if thisPlayerIsPlaying then
                inFront <| el [ alignRight, pointer, Events.onClick <| KggLeaveGame gameId, Font.size 14, moveDown 7 ] (text "❎")

              else
                noAttr
            ]
            (text "Partie en cours")
        , paragraph [ Font.center, width fill ] (List.map (el [ paddingXY 7 0 ] << playerView players) players)
        , row [ centerX ]
            [ el [ Font.size 18, Font.semiBold ] <| text "Score: "
            , el [ Font.size 16 ] <| text (String.fromInt substate.score)
            ]
        , mainKanjiView substate.roundLength substate.timeTillRoundEnd mbKanjiMeta substate.currentKanji
        , countDownView substate.startingCountdown substate.timeTillGameOver
        , if thisPlayerIsPlaying then
            addWordView gameId thisPlayer buffer wrongWord buffering

          else
            Element.none
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


mainKanjiView roundLength timeTillRoundEnd mbKanjiMeta kanji =
    let
        clampedRoundLength =
            if roundLength >= 0 then
                roundLength

            else
                0

        clampedTimeTillRoundEnd =
            if timeTillRoundEnd >= 0 then
                timeTillRoundEnd

            else
                0

        remainingTimeAngle =
            -pi / 2 + 2 * pi * (toFloat (clampedTimeTillRoundEnd - clampedRoundLength) / toFloat clampedRoundLength)

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
    column [ centerX, spacing 10 ]
        [ Maybe.map meaningsView mbKanjiMeta |> Maybe.withDefault Element.none
        , row [ spacing 0, width fill ]
            [ el
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
                    , if clampedRoundLength == clampedTimeTillRoundEnd then
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
            , Maybe.map
                (\k ->
                    column [ spacing 15, width (px 150) ]
                        [ (onYomiView << .readings) k
                        , (kunYomiView << .readings) k
                        ]
                )
                mbKanjiMeta
                |> Maybe.withDefault Element.none
            ]
        ]


meaningsView { kanji, meanings, coreMeanings, jitenon } =
    let
        source =
            if coreMeanings == [] then
                meanings

            else
                coreMeanings

        french =
            List.filter (\( l, m ) -> l == "fr") source
                |> List.map Tuple.second

        english =
            List.filter (\( l, m ) -> l == "en") source
                |> List.map Tuple.second

        msv =
            case french of
                [] ->
                    meaningView (String.join ", " english)

                _ ->
                    meaningView (String.join ", " french)

        meaningView m =
            case Html.Parser.run (String.Extra.toSentenceCase m) |> Result.map Html.Parser.Util.toVirtualDom of
                Ok xs ->
                    el [] (html <| Html.span [] xs)

                Err e ->
                    text (String.Extra.toSentenceCase m)
    in
    row [ spacing 5, width fill, centerX ]
        [ paragraph
            [ width fill, Font.size 18, Font.bold, Font.color darkRed, paddingEach { sides | right = 5 }, Font.center ]
            --(List.intersperse (text ", ") (List.map meaningView ms))
            [ msv ]
        ]


readingsView :
    List { rType : String, onType : Maybe String, rStatus : Maybe String, reading : String }
    -> Element FrontendMsg
readingsView rs =
    let
        onYomi =
            List.filter (\r -> r.rType == "ja_on") rs
                |> List.map .reading

        kunYomi =
            List.filter (\r -> r.rType == "ja_kun") rs
                |> List.map .reading

        kunYomiView_ ky =
            case String.split "." ky of
                prefix :: ending :: [] ->
                    row []
                        [ el [] (text prefix)
                        , el [ Font.color lightBlue ] (text ending)
                        ]

                _ ->
                    el [] (text ky)
    in
    column
        [ spacing 10
        , Font.size 16
        , width fill
        ]
        [ paragraph
            [ Font.center ]
            [ text <| String.join "、 " onYomi ]
        , paragraph
            [ Font.center ]
            (List.map kunYomiView_ kunYomi
                |> List.intersperse (el [] (text "、 "))
            )
        ]


onYomiView :
    List { rType : String, onType : Maybe String, rStatus : Maybe String, reading : String }
    -> Element FrontendMsg
onYomiView rs =
    let
        onYomi =
            List.filter (\r -> r.rType == "ja_on") rs
                |> List.map .reading
    in
    column
        [ spacing 10
        , Font.size 16
        , width fill
        , paddingXY 10 0
        ]
        [ paragraph
            [ Font.center, Font.color lightBlue ]
            [ text <| String.join "、 " onYomi ]
        ]


kunYomiView :
    List { rType : String, onType : Maybe String, rStatus : Maybe String, reading : String }
    -> Element FrontendMsg
kunYomiView rs =
    let
        kunYomi =
            List.filter (\r -> r.rType == "ja_kun") rs
                |> List.map .reading

        kunYomiView_ ky =
            case String.split "." ky of
                prefix :: ending :: [] ->
                    row []
                        [ el [] (text prefix)
                        , el [ Font.color lightBlue ] (text ending)
                        ]

                _ ->
                    el [] (text ky)
    in
    column
        [ spacing 10
        , Font.size 16
        , width fill
        , paddingXY 10 0
        ]
        [ paragraph
            [ Font.center ]
            (List.map kunYomiView_ kunYomi
                |> List.intersperse (el [] (text "、 "))
            )
        ]


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


addWordView : Int -> Player -> Maybe String -> Maybe String -> Bool -> Element FrontendMsg
addWordView gameId thisPlayer buffer wrongWord buffering =
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
                (buttonStyle_ (not <| buffering))
                { onPress =
                    if buffering then
                        Nothing

                    else
                        Just (KggRequestNextKanji gameId)
                , label = text "Suivant"
                }
            , Input.button
                (buttonStyle_ (buffer /= Nothing))
                { onPress = Maybe.map (\w -> KggSendWord gameId) buffer
                , label = text "Envoyer"
                }
            ]
        ]


endedView gameId players thisPlayer substate =
    let
        thisPlayerIsPlaying =
            List.member thisPlayer players
    in
    column
        [ padding 15
        , spacing 15
        , centerX
        ]
        [ el
            [ centerX
            , Font.size 22
            , Font.semiBold
            , paddingXY 60 0
            , if thisPlayerIsPlaying then
                inFront <| el [ alignRight, pointer, Events.onClick <| KggEndGame gameId, Font.size 14, moveDown 7 ] (text "❎")

              else
                noAttr
            ]
            (text "Game Over")
        , row [ centerX ] [ text "score: ", text (String.fromInt substate.score) ]
        , column [ width fill, spacing 15 ]
            (Dict.map (\k v -> column [ width fill, spacing 15 ] (List.map (jmDictEntryView () SearchEverything k) v))
                substate.words
                |> Dict.values
            )
        ]



-------------------------------------------------------------------------------


jmDictEntryView examplesLoadingStatus jmDictResultsLanguage searchStr e =
    column
        [ spacing 15
        , padding 15
        , width fill
        , Border.shadow
            { offset = ( 1, 1 )
            , size = 2
            , blur = 2
            , color = grey
            }
        , Border.rounded 10
        ]
        [ --el [ Font.bold ] (text <| String.fromInt e.ent_seq)
          --paragraph [] [ text <| Debug.toString e ]
          wrappedRow [ width fill ]
            [ paragraph [ width fill ]
                [ paragraph [ Font.size 20 ]
                    (List.map (k_eleView searchStr) e.k_ele
                        |> List.intersperse (text "; ")
                    )
                , paragraph [ Font.size 18 ]
                    (text "【"
                        :: (List.map (r_eleView searchStr) e.r_ele
                                |> List.intersperse (text "; ")
                           )
                        ++ [ text "】" ]
                    )
                ]

            --, loadExamplesView examplesLoadingStatus e searchStr
            ]
        , column [ spacing 10, width fill ]
            (List.concat
                [ [ wrappedRow [ spacing 10, width fill ]
                        (List.map posView
                            (List.concatMap .pos e.sense |> List.Extra.unique)
                        )
                  ]
                , List.map (senseView jmDictResultsLanguage) e.sense
                ]
            )
        , case e.sense of
            s :: ss ->
                if s.example == [] then
                    Element.none

                else
                    column
                        [ width fill, spacing 15, paddingEach { sides | top = 15 } ]
                        [--el [ Font.bold, Font.size 16 ] (text "Exemples:"), examplesView jmDictResultsLanguage s
                        ]

            _ ->
                Element.none
        ]


k_eleView searchStr { keb } =
    paragraph []
        (case
            String.split searchStr keb
                |> List.intersperse searchStr
                |> List.filter (\s -> s /= "")
         of
            [] ->
                [ text keb ]

            substrings ->
                List.map
                    (\sb ->
                        if sb == searchStr then
                            el [ Font.color lightBlue ] (text searchStr)

                        else
                            text sb
                    )
                    substrings
         --++ [ text <| " " ++ String.fromFloat (sift3Distance searchStr keb) ]
        )


r_eleView searchStr { reb, re_inf } =
    let
        outdated =
            List.member "out-dated or obsolete kana usage" re_inf
    in
    paragraph
        [ if outdated then
            alpha 0.5

          else
            noAttr
        ]
        (case
            String.split searchStr reb
                |> List.intersperse searchStr
                |> List.filter (\s -> s /= "")
         of
            [] ->
                [ text reb ]

            substrings ->
                List.map
                    (\sb ->
                        if sb == searchStr then
                            el [ Font.color lightBlue ] (text searchStr)

                        else
                            text sb
                    )
                    substrings
        )


senseView jmDictResultsLanguage ({ gloss, misc } as e) =
    let
        sfwGlosses =
            List.filter (\(Gloss v _) -> not (List.member "colloquialism" misc)) gloss

        frenchGlosses =
            List.filter (\(Gloss v { xmlLang }) -> xmlLang == Just "fre") gloss

        englishGlosses =
            List.filter (\(Gloss v { xmlLang }) -> xmlLang == Nothing) gloss

        glossView (Gloss v { xmlLang }) =
            row [ spacing 15 ] [ langView xmlLang, paragraph [] [ text v ] ]

        langView mbl =
            let
                attrs col =
                    [ paddingXY 6 4, Border.rounded 5, Background.color col, Font.size 14, Font.color white ]
            in
            if jmDictResultsLanguage == SearchEverything then
                case mbl of
                    Just "fre" ->
                        el (attrs lightGreen) (text "fr")

                    Nothing ->
                        el (attrs lightBlue) (text "en")

                    _ ->
                        Element.none

            else
                Element.none

        frenchView =
            List.map (\(Gloss v _) -> v) frenchGlosses
                |> List.intersperse ", "
                |> List.map text
                |> paragraph [ Font.size 16 ]
                |> (\p -> row [ spacing 15 ] [ langView (Just "fre"), p ])

        --List.map glossView frenchGlosses
        englishView =
            List.map (\(Gloss v _) -> v) englishGlosses
                |> List.intersperse ", "
                |> List.map text
                |> paragraph [ Font.size 16 ]
                |> (\p -> row [ spacing 15 ] [ langView Nothing, p ])

        --List.map glossView englishGlosses
        wrapper =
            identity

        --column
        --[ spacing 10, Font.size 16 ]
    in
    if frenchGlosses == [] && englishGlosses == [] then
        Element.none

    else
        case ( frenchGlosses, englishGlosses, jmDictResultsLanguage ) of
            ( x :: xs, _, SearchInFrench ) ->
                wrapper frenchView

            ( _, x :: xs, SearchInEnglish ) ->
                wrapper englishView

            ( _, _, SearchEverything ) ->
                if frenchGlosses /= [] then
                    wrapper frenchView

                else if englishGlosses /= [] then
                    wrapper englishView

                else
                    Element.none

            _ ->
                Element.none



--case relevantGlosses of
--    [] ->
--        Element.none
--    _ ->
--        column
--            [ spacing 10, Font.size 16 ]
--            (List.map glossView relevantGlosses)


posView pos =
    let
        attrs col =
            [ paddingXY 6 4
            , Border.rounded 5
            , Background.color col
            , Font.size 14
            , Font.color white

            --, width shrink
            ]
    in
    el (attrs blue) (entityView pos)


entityView s =
    case Dict.get s revEntityDict of
        Just { ref } ->
            text s

        --el [ htmlAttribute <| HtmlAttr.attribute "title" s ] (text ref)
        Nothing ->
            text s



-------------------------------------------------------------------------------


keyboardMsg model keyMsg =
    let
        pressedKeys =
            K.update keyMsg []
    in
    case getCurrentGame model of
        Just game ->
            ( model
            , if List.member K.Enter pressedKeys then
                toCmd <| KggSendWord game.gameId

              else if List.member K.Control pressedKeys then
                toCmd <| KggRequestNextKanji game.gameId

              else
                Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )



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
