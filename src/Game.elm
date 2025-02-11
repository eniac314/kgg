module Game exposing (..)

import ApiCalls
import Dict
import Helpers exposing (shuffleSeed)
import Lamdera exposing (broadcast, sendToFrontend)
import List.Extra
import Random
import Set
import Time
import Types exposing (..)


updateConfig :
    BackendModel
    -> GameId
    -> { kanjiSet : KanjiSet, roundLength : Int, startingCountdown : Int }
    -> ( BackendModel, Cmd BackendMsg )
updateConfig model gameId config =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                Lobby _ ->
                    let
                        newGame =
                            { game | gameState = Lobby config }
                    in
                    ( { model | kggames = Dict.insert gameId newGame model.kggames }
                    , broadcastGame newGame
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


createGame : BackendModel -> Time.Posix -> Player -> { kanjiSet : KanjiSet, roundLength : Int, startingCountdown : Int } -> ( BackendModel, Cmd BackendMsg )
createGame model now host config =
    let
        nextId =
            Dict.keys model.kggames
                |> List.foldr max -1
                |> (\n -> n + 1)

        newGame =
            { gameId = nextId
            , host = host
            , players = [ host ]
            , gameState = Lobby config
            , lastUpdated = Time.posixToMillis now
            , buffering = False
            , initialBuffer = False

            --, roundLength : Int
            }
    in
    ( { model | kggames = Dict.insert nextId newGame model.kggames }, broadcastGame newGame )


joinGame : BackendModel -> Player -> GameId -> ( BackendModel, Cmd BackendMsg )
joinGame model player gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            let
                newGame =
                    { game
                        | players =
                            if not <| List.member player game.players then
                                player :: game.players

                            else
                                game.players
                    }
            in
            ( { model | kggames = Dict.insert gameId newGame model.kggames }, broadcastGame newGame )

        _ ->
            ( model, Cmd.none )


leaveGame : BackendModel -> Player -> GameId -> ( BackendModel, Cmd BackendMsg )
leaveGame model player gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            let
                newGame =
                    { game
                        | players =
                            if List.member player game.players then
                                List.Extra.remove player game.players

                            else
                                game.players
                    }
            in
            ( { model | kggames = Dict.insert gameId newGame model.kggames }, broadcastGame newGame )

        _ ->
            ( model, Cmd.none )


launchPlay : BackendModel -> GameId -> ( BackendModel, Cmd BackendMsg )
launchPlay model gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                Lobby config ->
                    let
                        ( newSeed, kanjiForThisGame, bufferedKanji ) =
                            (\( s, r ) -> ( s, List.drop 5 r, List.take 5 r )) <|
                                case config.kanjiSet of
                                    JlptSet levels ->
                                        List.filterMap (\n -> Dict.get n jlpt) levels
                                            |> List.concatMap String.toList
                                            |> shuffleSeed model.seed

                                    CustomKanjiSet kanjiList ->
                                        ( model.seed, kanjiList )

                        currentKanji =
                            List.head bufferedKanji |> Maybe.withDefault '❌'

                        allTheWords =
                            Dict.empty

                        newGameState =
                            InPlay <|
                                { score = 0
                                , currentKanji = currentKanji
                                , remainingKanji = kanjiForThisGame
                                , bufferedKanji = List.tail bufferedKanji |> Maybe.withDefault []
                                , kanjiSeen = []
                                , words = Dict.empty
                                , allowedWords = allTheWords
                                , requestedSkip = []
                                , timeTillRoundEnd = config.roundLength
                                , timeTillGameOver = config.startingCountdown
                                , roundLength = config.roundLength
                                , startingCountdown = config.startingCountdown
                                }

                        newGame =
                            { game | gameState = newGameState, buffering = True, initialBuffer = True }
                    in
                    ( { model | kggames = Dict.insert gameId newGame model.kggames, seed = newSeed }
                    , Cmd.batch [ ApiCalls.getAllTheWords gameId bufferedKanji ]
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


runGames : BackendModel -> Time.Posix -> ( BackendModel, Cmd BackendMsg )
runGames model now =
    List.foldr
        (\g ( currentModel, cmds ) ->
            if gameRunning g then
                let
                    ( newModel, newCmd ) =
                        runGame currentModel now g.gameId
                in
                ( { currentModel | kggames = Dict.union newModel.kggames currentModel.kggames }
                , newCmd :: cmds
                )

            else
                ( currentModel, cmds )
        )
        ( model, [] )
        (Dict.values model.kggames)
        |> Tuple.mapSecond Cmd.batch


runGame : BackendModel -> Time.Posix -> GameId -> ( BackendModel, Cmd BackendMsg )
runGame model now gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            let
                timeStampedModel =
                    addTimeStampModel model gameId now
            in
            case game.gameState of
                Lobby config ->
                    launchPlay timeStampedModel gameId

                InPlay _ ->
                    toNextRound timeStampedModel gameId

                Victory _ ->
                    ( timeStampedModel, Cmd.none )

                GameOver _ ->
                    ( timeStampedModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


requestNextKanji : BackendModel -> GameId -> Player -> ( BackendModel, Cmd BackendMsg )
requestNextKanji model gameId player =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                InPlay substate ->
                    let
                        newGameState =
                            InPlay
                                { substate
                                    | requestedSkip =
                                        if not <| List.member player substate.requestedSkip then
                                            player :: substate.requestedSkip

                                        else
                                            substate.requestedSkip
                                }

                        newGame =
                            { game | gameState = newGameState }
                    in
                    ( { model | kggames = Dict.insert gameId newGame model.kggames }, broadcastGame newGame )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


allPlayerRequestedNextKanji game =
    case game.gameState of
        InPlay substate ->
            Set.fromList (List.map .id game.players) == Set.fromList (List.map .id substate.requestedSkip)

        _ ->
            False


addWord : BackendModel -> GameId -> Player -> String -> ( BackendModel, Cmd BackendMsg )
addWord model gameId player word =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                InPlay substate ->
                    let
                        currentWords =
                            Dict.get player.id substate.words |> Maybe.withDefault []

                        numberOfPlayers =
                            List.length game.players

                        timeBonus =
                            floor (10 / toFloat numberOfPlayers)

                        roundTimeBonus =
                            floor (5 / toFloat numberOfPlayers)

                        updateGame ns =
                            let
                                newGame =
                                    { game | gameState = InPlay ns }
                            in
                            ( { model | kggames = Dict.insert gameId newGame model.kggames }
                            , Cmd.batch
                                [ broadcastGame newGame
                                , if not <| canAddWord word game then
                                    sendToFrontend player.id (WrongWordTF word)

                                  else
                                    Cmd.none
                                ]
                            )

                        newSubstate =
                            if canAddWord word game then
                                { substate
                                    | score = substate.score + 10
                                    , words = Dict.insert player.id (word :: currentWords) substate.words
                                    , timeTillRoundEnd = substate.timeTillRoundEnd + roundTimeBonus
                                    , timeTillGameOver = substate.timeTillGameOver + timeBonus
                                }

                            else
                                { substate
                                    | timeTillRoundEnd = substate.timeTillRoundEnd - roundTimeBonus
                                    , timeTillGameOver = substate.timeTillGameOver - timeBonus
                                }
                    in
                    updateGame newSubstate

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


canAddWord : String -> KanjiGuessingGame -> Bool
canAddWord word game =
    case game.gameState of
        InPlay substate ->
            case Dict.get substate.currentKanji substate.allowedWords of
                Just relevantAllowedWords ->
                    (not <| List.member word (List.concat <| Dict.values substate.words))
                        && List.member word relevantAllowedWords

                _ ->
                    False

        _ ->
            False


toNextRound : BackendModel -> GameId -> ( BackendModel, Cmd BackendMsg )
toNextRound model gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                InPlay substate ->
                    if game.initialBuffer then
                        ( model, Cmd.none )

                    else
                        let
                            noMoreTime =
                                substate.timeTillGameOver <= 1

                            roundOver =
                                substate.timeTillRoundEnd <= 1

                            newGameState =
                                { substate
                                    | timeTillRoundEnd = max 0 <| substate.timeTillRoundEnd - 1
                                    , timeTillGameOver = max 0 <| substate.timeTillGameOver - 1
                                }

                            updateGame gs =
                                let
                                    newGame =
                                        { game | gameState = InPlay gs }
                                in
                                { model | kggames = Dict.insert gameId newGame model.kggames }
                        in
                        if noMoreTime then
                            let
                                newGame =
                                    { game | gameState = GameOver { score = substate.score } }
                            in
                            ( { model | kggames = Dict.insert gameId newGame model.kggames }, broadcastGame newGame )

                        else if allPlayerRequestedNextKanji game && (not <| game.buffering) then
                            loadNextKanji (updateGame { newGameState | requestedSkip = [] }) gameId

                        else if roundOver then
                            loadNextKanji (updateGame newGameState) gameId

                        else
                            ( updateGame newGameState, broadcastGameTimes { game | gameState = InPlay newGameState } )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


loadNextKanji : BackendModel -> GameId -> ( BackendModel, Cmd BackendMsg )
loadNextKanji model gameId =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                InPlay substate ->
                    let
                        mbNextKanjiToBuffer =
                            List.head substate.remainingKanji

                        updateGame ngs buffering cmd =
                            let
                                newGame =
                                    { game | gameState = ngs, buffering = buffering }
                            in
                            ( { model | kggames = Dict.insert gameId newGame model.kggames }
                            , Cmd.batch [ broadcastGame newGame, cmd ]
                            )
                    in
                    case substate.bufferedKanji of
                        [] ->
                            updateGame (Victory { score = substate.score }) False Cmd.none

                        nextKanji :: tail ->
                            updateGame
                                (InPlay
                                    { substate
                                        | currentKanji = nextKanji
                                        , kanjiSeen = substate.currentKanji :: substate.kanjiSeen
                                        , timeTillRoundEnd = substate.roundLength
                                        , remainingKanji = List.tail substate.remainingKanji |> Maybe.withDefault []
                                        , bufferedKanji =
                                            Maybe.map (\nkb -> tail ++ [ nkb ]) mbNextKanjiToBuffer
                                                |> Maybe.withDefault substate.bufferedKanji
                                    }
                                )
                                True
                                (Maybe.map (\nkb -> ApiCalls.getWords gameId nkb) mbNextKanjiToBuffer
                                    |> Maybe.withDefault Cmd.none
                                )

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


gameRunning : KanjiGuessingGame -> Bool
gameRunning game =
    case game.gameState of
        InPlay _ ->
            True

        _ ->
            False


gamesRunning : BackendModel -> Bool
gamesRunning { kggames } =
    List.any
        (\g ->
            case g.gameState of
                InPlay _ ->
                    True

                _ ->
                    False
        )
        (Dict.values kggames)


addTimeStamp : KanjiGuessingGame -> Time.Posix -> KanjiGuessingGame
addTimeStamp game now =
    { game | lastUpdated = Time.posixToMillis now }


addTimeStampModel : BackendModel -> GameId -> Time.Posix -> BackendModel
addTimeStampModel model gameId now =
    case Dict.get gameId model.kggames of
        Just game ->
            { model | kggames = Dict.insert gameId { game | lastUpdated = Time.posixToMillis now } model.kggames }

        _ ->
            model


broadcastGame : KanjiGuessingGame -> Cmd BackendMsg
broadcastGame game =
    let
        gameStateLight =
            case game.gameState of
                InPlay substate ->
                    InPlay { substate | allowedWords = Dict.empty, remainingKanji = [] }

                _ ->
                    game.gameState
    in
    broadcast <| GameBroadcastTF { game | gameState = gameStateLight }


broadcastGameTimes : KanjiGuessingGame -> Cmd BackendMsg
broadcastGameTimes game =
    case game.gameState of
        InPlay substate ->
            broadcast <|
                GameTimesBroadcastTF
                    { gameId = game.gameId
                    , lastUpdated = game.lastUpdated
                    , timeTillGameOver = substate.timeTillGameOver
                    , timeTillRoundEnd = substate.timeTillRoundEnd
                    , buffering = game.buffering
                    }

        _ ->
            Cmd.none



-------------------------------------------------------------------------------


randomJLPTKanjiList : Random.Seed -> Int -> ( Random.Seed, List Char )
randomJLPTKanjiList seed level =
    case Dict.get level jlpt of
        Just kanjiStr ->
            shuffleSeed seed (String.toList kanjiStr)

        _ ->
            ( seed, [] )


jlpt =
    Dict.fromList
        [ ( 5, "日一国人年大十二本中長出三時行見月分後前生五間上東四今金九入学高円子外八六下来気小七山話女北午百書先名川千水半男西電校語土木聞食車何南万毎白天母火右読友左休父雨店教売知起言" )
        , ( 4, "会同事自社発者地業方新場員立開手力問代明動京目通理体田主題意不作用度強公持野以思家世多正安院心界文元重近考画海道集別物使品計死特私始朝運終台広住無真有口少町料工建空急止送切転研足究楽着病質待試族銀早映親験英医仕去味写字答夜音注帰古歌買悪図週室歩風紙黒花春赤青館屋色走秋夏習駅洋旅服夕借曜飲肉貸堂鳥飯勉冬昼茶弟牛魚兄犬妹姉漢" )
        , ( 3, "政議民連対部合市内相定回選米実関決全表戦経最現調化当約首法性的要制治務成期取都和機平加受続進数記初指権支産点報済活原共得解交資予向際勝面告反判認参利組信在件側任引求所次昨論官増係感情投示変打直両式確果容必演歳争談能位置流格疑過局放常状球職与供役構割身費付由説難優夫収断石違消神番規術備宅害配警育席訪乗残想声助労例然限追商葉伝働形景落好退頭負渡失差末守若種美命福望非観察段横深申様財港識呼達良阪候程満敗値突光路科積他処太客否師登易速存飛殺号単座破除完降責捕危給苦迎園具辞因馬愛富彼未舞亡冷適婦寄込顔類余王返妻背熱宿薬険頼覚船途許抜便留罪努精散静婚喜浮絶幸押倒等老曲払庭徒勤遅居雑招困欠更刻賛抱犯恐息遠戻願絵越欲痛笑互束似列探逃遊迷夢君閉緒折草暮酒悲晴掛到寝暗盗吸陽御歯忘雪吹娘誤洗慣礼窓昔貧怒泳祖杯疲皆鳴腹煙眠怖耳頂箱晩寒髪忙才靴恥偶偉猫幾誰" )
        , ( 2, "党協総区領県設保改第結派府査委軍案策団各島革村勢減再税営比防補境導副算輸述線農州武象域額欧担準賞辺造被技低復移個門課脳極含蔵量型況針専谷史階管兵接細効丸湾録省旧橋岸周材戸央券編捜竹超並療採森競介根販歴将幅般貿講林装諸劇河航鉄児禁印逆換久短油暴輪占植清倍均億圧芸署伸停爆陸玉波帯延羽固則乱普測豊厚齢囲卒略承順岩練軽了庁城患層版令角絡損募裏仏績築貨混昇池血温季星永著誌庫刊像香坂底布寺宇巨震希触依籍汚枚複郵仲栄札板骨傾届巻燃跡包駐弱紹雇替預焼簡章臓律贈照薄群秒奥詰双刺純翌快片敬悩泉皮漁荒貯硬埋柱祭袋筆訓浴童宝封胸砂塩賢腕兆床毛緑尊祝柔殿濃液衣肩零幼荷泊黄甘臣浅掃雲掘捨軟沈凍乳恋紅郊腰炭踊冊勇械菜珍卵湖喫干虫刷湯溶鉱涙匹孫鋭枝塗軒毒叫拝氷乾棒祈拾粉糸綿汗銅湿瓶咲召缶隻脂蒸肌耕鈍泥隅灯辛磨麦姓筒鼻粒詞胃畳机膚濯塔沸灰菓帽枯涼舟貝符憎皿肯燥畜坊挟曇滴伺" )
        ]
