module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Codec
import Dict exposing (..)
import Http
import Json.Decode as D
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import Random exposing (Seed)
import Set exposing (Set)
import Time exposing (Posix)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , kggames : Dict Int KanjiGuessingGame
    , username : Maybe String
    , thisPlayer : Maybe Player
    , currentlyPlaying : Maybe GameId
    , players : List Player
    , kggWordInput : Maybe String
    , kggWrongWordBuffer : Maybe String
    , kggConfigInputs :
        { kggRandomKanjiInput : Maybe String
        , kggRoundLengthInput : Maybe String
        , kggStartingCountdownInput : Maybe String
        }
    , isEmbedded : Maybe Bool
    , now : Time.Posix
    }


type FrontEndStatus
    = WaitingForEmbedTest
    | WaitingForPlayerInfo
    | WaitingForInitialGamesBroadcast


type alias BackendModel =
    { message : String
    , kggames : Dict Int KanjiGuessingGame
    , players :
        Dict
            SessionId
            { player : Player
            , phpSessionId : PhpSessionId
            }
    , seed : Seed
    }


type alias Username =
    String


type alias PhpSessionId =
    String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | GotInfoFromParent String
    | GotIFrameTestResult String
    | UsernameInput String
    | SendUser
    | ReqGetKey
    | KggSetCustomKanjiSet String
    | KggSetKanjiSet KanjiSet Int
    | KggStartGame GameId
    | KggLoadInitialData GameId
    | KggWordInput String
    | KggHostGame
    | KggJoinGame Int
    | KggLeaveGame Int
    | KggSendWord GameId
    | KggRequestNextKanji GameId
    | GotTimeF Time.Posix
    | SendToBackendWithTime ToBackend
    | KeyboardMsg Keyboard.Msg
    | NoOpFrontendMsg


type ToBackend
    = GetKeysTB
    | PlayerInfoSubmittedTB Username PhpSessionId
    | RequestInitialGamesBroadCastTB
    | CreateGameTB Player { kanjiSet : KanjiSet, roundLength : Int, startingCountdown : Int } Time.Posix
    | JoinTB Player GameId
    | LeaveTB Player GameId
    | UpdateConfigTB GameId { kanjiSet : KanjiSet, roundLength : Int, startingCountdown : Int }
    | LoadInitialDataTB GameId Time.Posix
    | StartTB GameId Time.Posix
    | RequestNextKanjiTB GameId Player
    | AddWordTB GameId Player String
    | NoOpTB


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | GotJMdictSearchResults GameId Char (Result Http.Error (List JapDictEntry))
    | GetKeys
    | GotKeys (Result Http.Error String)
    | GotTime Time.Posix
      --| LaunchGame GameId
    | RunGames Time.Posix
    | NoOpBackendMsg


type ToFrontend
    = ToFrontendMsgTF String
      --| InitialBufferTF GameId { bufferSize : Int, done : Set Char }
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



-------------------------------------------------------------------------------


type Status
    = Initial
    | Waiting
    | Success
    | Failure



-------------------------------------------------------------------------------


type alias Player =
    { id : ClientId
    , name : Username
    }


type alias PlayerId =
    Int


type alias GameId =
    Int


type alias KanjiGuessingGame =
    { gameId : Int
    , host : Player
    , players : List Player
    , gameState : KGGameState
    , lastUpdated : Int
    , buffering : Bool
    }


type KGGameState
    = Lobby
        { kanjiSet : KanjiSet
        , roundLength : Int
        , startingCountdown : Int
        }
    | Loading
        { initialBuffer : { bufferSize : Int, done : Set Char }
        , currentKanji : Char
        , remainingKanji : List Char
        , bufferedKanji : List Char
        , allowedWords : Dict Char (List String)
        , timeTillRoundEnd : Int
        , timeTillGameOver : Int
        , roundLength : Int
        , startingCountdown : Int
        }
    | InPlay
        { score : Int
        , currentKanji : Char
        , remainingKanji : List Char
        , bufferedKanji : List Char
        , kanjiSeen : List Char
        , words : Dict ClientId (List String)
        , allowedWords : Dict Char (List String)
        , requestedSkip : List Player
        , timeTillRoundEnd : Int
        , timeTillGameOver : Int
        , roundLength : Int
        , startingCountdown : Int
        }
    | Victory { score : Int }
    | GameOver { score : Int }


type KanjiSet
    = JlptSet (List Int)
    | CustomKanjiSet (List Char)



-------------------------------------------------------------------------------


type alias KanjidicEntry =
    { kanji : String
    , cpValues : List ( String, String )
    , radValues : List ( String, Int )

    -- misc
    , grade : Maybe Int
    , strokeCount : List Int
    , variants : List ( ( String, String ), Maybe String )
    , freq : Maybe Int
    , radName : List String
    , jlpt : Maybe Int

    --
    , dicNumber : List ( String, String )
    , queryCode : List ( String, String )
    , skipMissclass : List ( String, String )

    -- reading/meaning
    , readings : List { rType : String, onType : Maybe String, rStatus : Maybe String, reading : String }
    , meanings : List ( String, String )
    , nanori : List String

    -- custom fields
    , coreMeanings : List ( String, String )
    , examples : List ( String, String )
    , decomposition : Maybe String
    , etymology : Maybe { hint : Maybe String, etym : Maybe String }
    }



-------------------------------------------------------------------------------
-- JMDict


type alias JapDictEntry =
    { ent_seq : Int
    , k_ele : List KanjiElement
    , r_ele : List ReadingElement
    , sense : List Sense
    , showingEverySense : Bool
    }


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


type LSource
    = LSource
        String
        { xmlLang : Maybe String -- def eng
        , ls_type : Maybe String
        , ls_wasei : Bool
        }


type Gloss
    = Gloss
        String
        { xmlLang : Maybe String -- def eng
        , g_gend : Maybe String
        , g_type : Maybe String
        }


type alias Example =
    { ex_srce : String
    , ex_text : String
    , ex_sent : List Ex_sent
    }


type Ex_sent
    = Ex_sent
        String
        { xmlLang : Maybe String
        , ex_srce : Maybe String
        }


type JMdictLanguage
    = SearchInFrench
    | SearchInEnglish
    | SearchEverything


type alias JMdictFromJapSearch =
    { searchStr : String
    , hasKanji : Bool
    , targetLanguage : JMdictLanguage
    }


type alias JMdictToJapSearch =
    { searchStr : String
    , targetLanguage : JMdictLanguage
    }


type alias TatoebaExample =
    { text : String
    , translations : List TatoebaTranslation
    , transcriptions : List TatoebaTranscription
    }


type alias TatoebaTranslation =
    ( String, String )


type alias TatoebaTranscription =
    String
