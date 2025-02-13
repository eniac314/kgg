module Backend exposing (..)

--import DebugApp
--import Debuggy.App

import ApiCalls exposing (..)
import Delay
import Dict
import Game exposing (..)
import Helpers exposing (..)
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Random
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        --NoOpBackendMsg
        --"2238138fd1560b4c"
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions =
            \m ->
                if gamesRunning m then
                    Time.every 1000 RunGames

                else
                    Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = ""
      , kggames = Dict.empty
      , players = Dict.empty
      , seed = Random.initialSeed 0
      }
      --, getWords '語'
      --, Cmd.none
    , Cmd.batch
        [ Task.perform GotTime Time.now ]
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        -- A new client has joined! Send them history, and let everyone know
        ClientConnected sessionId clientId ->
            ( model
              --{model | players =}
            , Cmd.batch []
              --[ broadcast (MessageReceived (Joined clientId))
              --, sendToFrontend clientId (HistoryReceived model.messages)
              --]
            )

        -- A client has disconnected, let everyone know
        ClientDisconnected sessionId clientId ->
            ( model
            , Cmd.none
            )

        --broadcast (MessageReceived (Left clientId)) )
        GotJMdictSearchResults gameId kanji res ->
            gotJMdictSearchResults model gameId kanji res

        GetKeys ->
            ( model, Cmd.batch [ getKanjiKeys ] )

        GotKeys res ->
            case res of
                Ok s ->
                    ( { model | message = s }
                    , broadcast (ToFrontendMsgTF s)
                    )

                Err e ->
                    ( model
                    , broadcast (ToFrontendMsgTF (httpErrorToString e))
                    )

        GotTime posix ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis posix) }, Cmd.none )

        RunGames now ->
            runGames model now

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GetKeysTB ->
            ( model, Cmd.none )

        CreateGameTB host config now ->
            createGame model now host config

        JoinTB player gameId ->
            joinGame model player gameId

        LeaveTB player gameId ->
            leaveGame model player gameId

        UpdateConfigTB gameId config ->
            updateConfig model gameId config

        StartTB gameId now ->
            runGame model now gameId

        RequestNextKanjiTB gameId player ->
            requestNextKanji model gameId player

        AddWordTB gameId player word ->
            addWord model gameId player word

        PlayerInfoSubmittedTB username phpSessionId ->
            ( { model
                | players =
                    Dict.insert sessionId
                        { player = { name = username, id = clientId }
                        , phpSessionId = phpSessionId
                        }
                        model.players
              }
            , Lamdera.sendToFrontend clientId <| PlayerInfoRegisteredTF { name = username, id = clientId }
            )

        RequestInitialGamesBroadCastTB ->
            ( model, initialGamesBroadcast model clientId )

        NoOpTB ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
