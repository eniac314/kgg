module Evergreen.V4.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Lamdera
import Random
import Time
import Url


type alias Username =
    String


type alias Player =
    { id : Lamdera.ClientId
    , name : Username
    }


type KanjiSet
    = JlptSet (List Int)
    | CustomKanjiSet (List Char)


type KGGameState
    = Lobby
        { kanjiSet : KanjiSet
        , roundLength : Int
        , startingCountdown : Int
        }
    | InPlay
        { score : Int
        , currentKanji : Char
        , remainingKanji : List Char
        , bufferedKanji : List Char
        , kanjiSeen : List Char
        , words : Dict.Dict Lamdera.ClientId (List String)
        , allowedWords : Dict.Dict Char (List String)
        , requestedSkip : List Player
        , timeTillRoundEnd : Int
        , timeTillGameOver : Int
        , roundLength : Int
        , startingCountdown : Int
        }
    | Victory
        { score : Int
        }
    | GameOver
        { score : Int
        }


type alias KanjiGuessingGame =
    { gameId : Int
    , host : Player
    , players : List Player
    , gameState : KGGameState
    , lastUpdated : Int
    , buffering : Bool
    , initialBuffer : Bool
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , kggames : Dict.Dict Int KanjiGuessingGame
    , thisPlayer : Maybe Player
    , players : List Player
    , kggWordInput : Maybe String
    , kggWrongWordBuffer : Maybe String
    , kggConfigInputs :
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    , kggSyncing : Bool
    , now : Time.Posix
    }


type alias PhpSessionId =
    String


type alias BackendModel =
    { message : String
    , kggames : Dict.Dict Int KanjiGuessingGame
    , players :
        Dict.Dict
            Lamdera.SessionId
            { player : Player
            , phpSessionId : PhpSessionId
            }
    , seed : Random.Seed
    }


type alias GameId =
    Int


type ToBackend
    = GetKeysTB
    | PlayerInfoSubmittedTB Username PhpSessionId
    | CreateGameTB
        Player
        { kanjiSet : KanjiSet
        , roundLength : Int
        , startingCountdown : Int
        }
        Time.Posix
    | JoinTB Player GameId
    | LeaveTB Player GameId
    | UpdateConfigTB
        GameId
        { kanjiSet : KanjiSet
        , roundLength : Int
        , startingCountdown : Int
        }
    | StartTB GameId Time.Posix
    | RequestNextKanjiTB GameId Player
    | AddWordTB GameId Player String
    | NoOpTB


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotInfoFromParent String
    | ReqGetKey
    | KggSetCustomKanjiSet String
    | KggSetKanjiSet KanjiSet Int
    | KggStartGame GameId
    | KggWordInput String
    | KggHostGame
    | KggJoinGame Int
    | KggLeaveGame Int
    | KggSendWord GameId
    | KggRequestNextKanji GameId
    | GotTimeF Time.Posix
    | SendToBackendWithTime ToBackend
    | NoOpFrontendMsg


type alias KanjiElement =
    { keb : String
    , k_inf : List String
    , k_pri : List String
    }


type alias ReadingElement =
    { reb : String
    , re_nokanji : Bool
    , re_restr : List String
    , re_inf : List String
    , re_pri : List String
    }


type LSource
    = LSource
        String
        { xmlLang : Maybe String
        , ls_type : Maybe String
        , ls_wasei : Bool
        }


type Gloss
    = Gloss
        String
        { xmlLang : Maybe String
        , g_gend : Maybe String
        , g_type : Maybe String
        }


type Ex_sent
    = Ex_sent
        String
        { xmlLang : Maybe String
        , ex_srce : Maybe String
        }


type alias Example =
    { ex_srce : String
    , ex_text : String
    , ex_sent : List Ex_sent
    }


type alias Sense =
    { stagk : List String
    , stagr : List String
    , pos : List String
    , xref : List String
    , ant : List String
    , field : List String
    , misc : List String
    , s_inf : List String
    , lsource : List LSource
    , dial : List String
    , gloss : List Gloss
    , example : List Example
    }


type alias JapDictEntry =
    { ent_seq : Int
    , k_ele : List KanjiElement
    , r_ele : List ReadingElement
    , sense : List Sense
    , showingEverySense : Bool
    }


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | GotJMdictSearchResults GameId Char (Result Http.Error (List JapDictEntry))
    | GetKeys
    | GotKeys (Result Http.Error String)
    | GotTime Time.Posix
    | RunGames Time.Posix
    | NoOpBackendMsg


type ToFrontend
    = ToFrontendMsgTF String
    | GameBroadcastTF KanjiGuessingGame
    | GameTimesBroadcastTF
        { gameId : GameId
        , lastUpdated : Int
        , timeTillGameOver : Int
        , timeTillRoundEnd : Int
        , buffering : Bool
        }
    | WrongWordTF String
    | PlayerInfoRegisteredTF Player
    | NoOpTF
