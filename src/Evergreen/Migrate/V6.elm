module Evergreen.Migrate.V6 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Dict
import Evergreen.V4.Types
import Evergreen.V6.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V4.Types.FrontendModel -> ModelMigration Evergreen.V6.Types.FrontendModel Evergreen.V6.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V4.Types.BackendModel -> ModelMigration Evergreen.V6.Types.BackendModel Evergreen.V6.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V4.Types.FrontendMsg -> MsgMigration Evergreen.V6.Types.FrontendMsg Evergreen.V6.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V4.Types.ToBackend -> MsgMigration Evergreen.V6.Types.ToBackend Evergreen.V6.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V4.Types.BackendMsg -> MsgMigration Evergreen.V6.Types.BackendMsg Evergreen.V6.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V4.Types.ToFrontend -> MsgMigration Evergreen.V6.Types.ToFrontend Evergreen.V6.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V4.Types.FrontendModel -> Evergreen.V6.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , message = old.message
    , kggames = old.kggames |> Dict.map (\k -> migrate_Types_KanjiGuessingGame)
    , username = Nothing
    , thisPlayer = old.thisPlayer
    , players = old.players
    , kggWordInput = old.kggWordInput
    , kggWrongWordBuffer = old.kggWrongWordBuffer
    , kggConfigInputs =
        old.kggConfigInputs
            |> (\rec -> rec)
    , kggSyncing = old.kggSyncing
    , isEmbedded = Nothing
    , now = old.now
    }


migrate_Types_FrontendMsg : Evergreen.V4.Types.FrontendMsg -> Evergreen.V6.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V4.Types.UrlClicked p0 ->
            Evergreen.V6.Types.UrlClicked p0

        Evergreen.V4.Types.UrlChanged p0 ->
            Evergreen.V6.Types.UrlChanged p0

        Evergreen.V4.Types.GotInfoFromParent p0 ->
            Evergreen.V6.Types.GotInfoFromParent p0

        Evergreen.V4.Types.ReqGetKey ->
            Evergreen.V6.Types.ReqGetKey

        Evergreen.V4.Types.KggSetCustomKanjiSet p0 ->
            Evergreen.V6.Types.KggSetCustomKanjiSet p0

        Evergreen.V4.Types.KggSetKanjiSet p0 p1 ->
            Evergreen.V6.Types.KggSetKanjiSet (p0 |> migrate_Types_KanjiSet) p1

        Evergreen.V4.Types.KggStartGame p0 ->
            Evergreen.V6.Types.KggStartGame p0

        Evergreen.V4.Types.KggWordInput p0 ->
            Evergreen.V6.Types.KggWordInput p0

        Evergreen.V4.Types.KggHostGame ->
            Evergreen.V6.Types.KggHostGame

        Evergreen.V4.Types.KggJoinGame p0 ->
            Evergreen.V6.Types.KggJoinGame p0

        Evergreen.V4.Types.KggLeaveGame p0 ->
            Evergreen.V6.Types.KggLeaveGame p0

        Evergreen.V4.Types.KggSendWord p0 ->
            Evergreen.V6.Types.KggSendWord p0

        Evergreen.V4.Types.KggRequestNextKanji p0 ->
            Evergreen.V6.Types.KggRequestNextKanji p0

        Evergreen.V4.Types.GotTimeF p0 ->
            Evergreen.V6.Types.GotTimeF p0

        Evergreen.V4.Types.SendToBackendWithTime p0 ->
            Evergreen.V6.Types.SendToBackendWithTime (p0 |> migrate_Types_ToBackend)

        Evergreen.V4.Types.NoOpFrontendMsg ->
            Evergreen.V6.Types.NoOpFrontendMsg


migrate_Types_KGGameState : Evergreen.V4.Types.KGGameState -> Evergreen.V6.Types.KGGameState
migrate_Types_KGGameState old =
    case old of
        Evergreen.V4.Types.Lobby p0 ->
            Evergreen.V6.Types.Lobby
                { kanjiSet = p0.kanjiSet |> migrate_Types_KanjiSet
                , roundLength = p0.roundLength
                , startingCountdown = p0.startingCountdown
                }

        Evergreen.V4.Types.InPlay p0 ->
            Evergreen.V6.Types.InPlay p0

        Evergreen.V4.Types.Victory p0 ->
            Evergreen.V6.Types.Victory p0

        Evergreen.V4.Types.GameOver p0 ->
            Evergreen.V6.Types.GameOver p0


migrate_Types_KanjiGuessingGame : Evergreen.V4.Types.KanjiGuessingGame -> Evergreen.V6.Types.KanjiGuessingGame
migrate_Types_KanjiGuessingGame old =
    { gameId = old.gameId
    , host = old.host |> migrate_Types_Player
    , players = old.players
    , gameState = old.gameState |> migrate_Types_KGGameState
    , lastUpdated = old.lastUpdated
    , buffering = old.buffering
    , initialBuffer = old.initialBuffer
    }


migrate_Types_KanjiSet : Evergreen.V4.Types.KanjiSet -> Evergreen.V6.Types.KanjiSet
migrate_Types_KanjiSet old =
    case old of
        Evergreen.V4.Types.JlptSet p0 ->
            Evergreen.V6.Types.JlptSet p0

        Evergreen.V4.Types.CustomKanjiSet p0 ->
            Evergreen.V6.Types.CustomKanjiSet p0


migrate_Types_Player : Evergreen.V4.Types.Player -> Evergreen.V6.Types.Player
migrate_Types_Player old =
    old


migrate_Types_ToBackend : Evergreen.V4.Types.ToBackend -> Evergreen.V6.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V4.Types.GetKeysTB ->
            Evergreen.V6.Types.GetKeysTB

        Evergreen.V4.Types.PlayerInfoSubmittedTB p0 p1 ->
            Evergreen.V6.Types.PlayerInfoSubmittedTB p0 p1

        Evergreen.V4.Types.CreateGameTB p0 p1 p2 ->
            Evergreen.V6.Types.CreateGameTB (p0 |> migrate_Types_Player)
                { kanjiSet = p1.kanjiSet |> migrate_Types_KanjiSet
                , roundLength = p1.roundLength
                , startingCountdown = p1.startingCountdown
                }
                p2

        Evergreen.V4.Types.JoinTB p0 p1 ->
            Evergreen.V6.Types.JoinTB (p0 |> migrate_Types_Player) p1

        Evergreen.V4.Types.LeaveTB p0 p1 ->
            Evergreen.V6.Types.LeaveTB (p0 |> migrate_Types_Player) p1

        Evergreen.V4.Types.UpdateConfigTB p0 p1 ->
            Evergreen.V6.Types.UpdateConfigTB p0
                { kanjiSet = p1.kanjiSet |> migrate_Types_KanjiSet
                , roundLength = p1.roundLength
                , startingCountdown = p1.startingCountdown
                }

        Evergreen.V4.Types.StartTB p0 p1 ->
            Evergreen.V6.Types.StartTB p0 p1

        Evergreen.V4.Types.RequestNextKanjiTB p0 p1 ->
            Evergreen.V6.Types.RequestNextKanjiTB p0 (p1 |> migrate_Types_Player)

        Evergreen.V4.Types.AddWordTB p0 p1 p2 ->
            Evergreen.V6.Types.AddWordTB p0 (p1 |> migrate_Types_Player) p2

        Evergreen.V4.Types.NoOpTB ->
            Evergreen.V6.Types.NoOpTB
