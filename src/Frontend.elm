port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.HexColor exposing (rgbCSSHex)
import Element.Input as Input
import GuessingGame as Kgg
import Helpers exposing (..)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as D
import Json.Encode as E
import Keyboard as K
import Lamdera
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Task
import Time
import Types exposing (..)
import Url


port toParentPort : E.Value -> Cmd msg


port fromParentPort : (String -> msg) -> Sub msg


port isIFrameTestPort : (String -> msg) -> Sub msg


fromParent model jsonStr =
    case D.decodeString fromParentPayload jsonStr of
        Ok (UserInfoPayload sessionCookie username) ->
            ( model, Lamdera.sendToBackend <| PlayerInfoSubmittedTB username sessionCookie )

        Err _ ->
            ( model, Cmd.none )


gotIFrameTestResult : Model -> String -> ( Model, Cmd FrontendMsg )
gotIFrameTestResult model jsonStr =
    case D.decodeString (D.field "isInIframe" D.bool) jsonStr of
        Ok res ->
            ( { model | isEmbedded = Just res }, Cmd.none )

        Err e ->
            let
                d =
                    Debug.log "" e
            in
            ( model, Cmd.none )


type FromParentPayload
    = UserInfoPayload String String


fromParentPayload =
    D.oneOf [ userInfoPayload ]


userInfoPayload =
    D.map2 UserInfoPayload
        (D.field "cookie" D.string)
        (D.field "username" D.string)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \m ->
                Sub.batch
                    [ fromParentPort GotInfoFromParent
                    , isIFrameTestPort GotIFrameTestResult
                    , Sub.map KeyboardMsg K.subscriptions
                    ]
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = ""
      , kggames = Dict.empty
      , username = Nothing
      , thisPlayer = Nothing
      , currentlyPlaying = Nothing
      , players = []
      , kggWordInput = Nothing
      , kggWrongWordBuffer = Nothing
      , kggConfigInputs =
            { kggRandomKanjiInput = Nothing
            , kggRoundLengthInput = Nothing
            , kggStartingCountdownInput = Nothing
            }
      , isEmbedded = Nothing
      , now = Time.millisToPosix 0
      }
    , Cmd.none
      --Cmd.batch [ Lamdera.sendToBackend <| PlayerInfoSubmittedTB "florian" "" ]
      --, getKanjiKeys
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        GotInfoFromParent jsonStr ->
            fromParent model jsonStr

        GotIFrameTestResult jsonStr ->
            gotIFrameTestResult model jsonStr

        UsernameInput name ->
            ( { model | username = mbStr name }, Cmd.none )

        SendUser ->
            case model.username of
                Just u ->
                    ( model, Lamdera.sendToBackend <| PlayerInfoSubmittedTB u "" )

                Nothing ->
                    ( model, Cmd.none )

        ReqGetKey ->
            ( model, Lamdera.sendToBackend GetKeysTB )

        KggSetCustomKanjiSet kanjiStr ->
            Kgg.setCustomKanjiSet model kanjiStr

        KggSetKanjiSet kanjiSet gameId ->
            Kgg.setKanjiSet model kanjiSet gameId

        KggHostGame ->
            Kgg.hostGame model

        KggJoinGame gameId ->
            Kgg.joinGame model gameId

        KggLeaveGame gameId ->
            Kgg.leaveGame model gameId

        KggWordInput word ->
            Kgg.wordInput model word

        KggLoadInitialData gameId ->
            Kgg.loadInitialData model gameId

        KggStartGame gameId ->
            Kgg.startGame model gameId

        KggRequestNextKanji gameId ->
            Kgg.requestNextKanji model gameId

        KggSendWord gameId ->
            Kgg.sendWord model gameId

        --GotKeys res ->
        --    case res of
        --        Ok s ->
        --            ( { model | message = s }
        --            , Cmd.none
        --            )
        --        _ ->
        --            ( model, Cmd.none )
        GotTimeF time ->
            ( { model | now = time }, Cmd.none )

        SendToBackendWithTime toBackend ->
            ( model, Lamdera.sendToBackend toBackend )

        KeyboardMsg keyMsg ->
            Kgg.keyboardMsg model keyMsg

        NoOpFrontendMsg ->
            ( model, Cmd.none )


baseUrl =
    "https://www.uminokirin.com"


getKanjiKeys =
    Http.get
        { url = baseUrl ++ "/api/getKanjiKeys"
        , expect = Http.expectString GotKeys
        }


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ToFrontendMsgTF s ->
            ( { model | message = s }, Cmd.none )

        GameBroadcastTF game ->
            let
                newModel =
                    { model
                        | kggames = Dict.insert game.gameId game model.kggames
                        , kggWordInput = Nothing
                    }
            in
            case game.gameState of
                Loading substate ->
                    if Kgg.currentGameInitialLoadingStatus substate == 100 then
                        Kgg.startGame newModel game.gameId

                    else
                        ( newModel, Cmd.none )

                _ ->
                    ( newModel, Cmd.none )

        GameTimesBroadcastTF data ->
            case Dict.get data.gameId model.kggames of
                Just game ->
                    case game.gameState of
                        InPlay substate ->
                            let
                                newGameState =
                                    InPlay
                                        { substate
                                            | timeTillRoundEnd = data.timeTillRoundEnd
                                            , timeTillGameOver = data.timeTillGameOver
                                        }

                                newGame =
                                    { game | lastUpdated = data.lastUpdated, gameState = newGameState, buffering = data.buffering }

                                d =
                                    Debug.log "newGame" newGame
                            in
                            ( { model | kggames = Dict.insert data.gameId newGame model.kggames }, Cmd.none )

                        _ ->
                            let
                                d =
                                    Debug.log "not in play" data
                            in
                            ( model, Cmd.none )

                _ ->
                    let
                        d =
                            Debug.log "game missing" data
                    in
                    ( model, Cmd.none )

        WrongWordTF wrongWord ->
            ( { model | kggWrongWordBuffer = Just wrongWord }, Cmd.none )

        PlayerInfoRegisteredTF player ->
            ( { model | thisPlayer = Just player }, Lamdera.sendToBackend RequestInitialGamesBroadCastTB )

        NoOpTF ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [] <|
            case model.thisPlayer of
                Just player ->
                    Kgg.view model

                Nothing ->
                    case model.isEmbedded of
                        Just True ->
                            Kgg.view model

                        Just False ->
                            userInputView model

                        Nothing ->
                            el [ padding 15 ] <| text "Loading..."
        ]
    }


userInputView model =
    column
        [ padding 15 ]
        [ column [ spacing 15 ]
            [ Input.text []
                { onChange = UsernameInput
                , text = model.username |> Maybe.withDefault ""
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Username"
                }
            , Input.button (buttonStyle_ (model.username /= Nothing))
                { onPress = Maybe.map (always SendUser) model.username, label = text "Envoyer" }
            ]
        ]
