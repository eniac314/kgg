module ApiCalls exposing (..)

--import DebugApp
--import Debuggy.App

import Codec exposing (..)
import Delay
import Dict
import Env
import Helpers exposing (httpErrorToString)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Parser exposing (..)
import Set
import StringDistance exposing (sift3Distance)
import Types exposing (..)


gotJMdictSearchResults : BackendModel -> GameId -> Char -> Result Http.Error (List JapDictEntry) -> ( BackendModel, Cmd BackendMsg )
gotJMdictSearchResults model gameId kanji res =
    case Dict.get gameId model.kggames of
        Just game ->
            case game.gameState of
                InPlay substate ->
                    case res of
                        Ok entries ->
                            let
                                relevance e =
                                    List.map (\k -> sift3Distance k.keb (String.fromChar kanji)) e.k_ele
                                        |> List.minimum
                                        |> Maybe.withDefault 100000

                                sortedEntries =
                                    List.sortBy relevance entries
                                        |> List.concatMap .k_ele
                                        |> List.map .keb

                                newSubstate =
                                    { substate
                                        | allowedWords = Dict.insert kanji sortedEntries substate.allowedWords
                                        , initialBuffer = (\ib -> { ib | done = Set.insert kanji ib.done }) substate.initialBuffer
                                    }

                                newGame =
                                    { game
                                        | gameState = InPlay newSubstate
                                        , buffering = not isDone

                                        --if game.initialBuffer then
                                        --    not isDone
                                        --else
                                        --    False
                                    }

                                isDone =
                                    List.all (\k -> List.member k (Dict.keys newSubstate.allowedWords)) substate.bufferedKanji

                                gameStateLight =
                                    case newGame.gameState of
                                        InPlay st ->
                                            InPlay { st | allowedWords = Dict.empty, remainingKanji = [] }

                                        _ ->
                                            newGame.gameState

                                broadcastIfDone =
                                    if isDone then
                                        broadcast <| GameBroadcastTF { newGame | gameState = gameStateLight }

                                    else
                                        Cmd.none
                            in
                            ( { model | kggames = Dict.insert gameId newGame model.kggames }
                            , if isDone then
                                broadcast <| GameBroadcastTF { newGame | gameState = gameStateLight }

                              else
                                broadcast <| InitialBufferTF gameId newSubstate.initialBuffer
                            )

                        Err e ->
                            ( model, broadcast (ToFrontendMsgTF (httpErrorToString e)) )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


baseUrl =
    case Env.mode of
        Env.Production ->
            "https://www.uminokirin.com"

        Env.Development ->
            "http://localhost:8001/https://www.uminokirin.com"


getKanjiKeys =
    --let
    --    d =
    --        Debug.log "test" "getKanjiKeys"
    --in
    Http.get
        { url = baseUrl ++ "/api/getKanjiKeys"
        , expect = Http.expectString GotKeys
        }


getWords : GameId -> Char -> Cmd BackendMsg
getWords gameId kanji =
    --let
    --    d =
    --        Debug.log "ApiCalls" "getWords"
    --in
    Http.post
        { url = baseUrl ++ "/api/jmDictSearchFromJap"
        , body =
            E.object
                [ ( "searchStr", E.string (String.fromChar kanji) )
                , ( "hasKanji", E.bool True )
                , ( "targetLanguage", E.string "SearchInFrench" )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson (GotJMdictSearchResults gameId kanji) (D.list (Codec.decoder japDictEntryCodec))
        }


getAllTheWords : GameId -> List Char -> Cmd BackendMsg
getAllTheWords gameId kanjiList =
    List.map (getWords gameId) kanjiList
        |> Cmd.batch



-------------------------------------------------------------------------------
-- Codec


japDictEntryCodec : Codec JapDictEntry
japDictEntryCodec =
    Codec.object (\a b c d -> JapDictEntry a b c d False)
        |> Codec.field "ent_seq" .ent_seq Codec.int
        |> Codec.field "k_ele" .k_ele (Codec.list kanjiElementCodec)
        |> Codec.field "r_ele" .r_ele (Codec.list readingElementCodec)
        |> Codec.field "sense" .sense (Codec.list senseCodec)
        |> Codec.buildObject


kanjiElementCodec : Codec KanjiElement
kanjiElementCodec =
    Codec.object KanjiElement
        |> Codec.field "keb" .keb Codec.string
        |> Codec.field "k_inf" .k_inf (Codec.list entityCodec)
        |> Codec.field "k_pri" .k_pri (Codec.list Codec.string)
        |> Codec.buildObject


readingElementCodec : Codec ReadingElement
readingElementCodec =
    Codec.object ReadingElement
        |> Codec.field "reb" .reb Codec.string
        |> Codec.field "re_nokanji" .re_nokanji Codec.bool
        |> Codec.field "re_restr" .re_restr (Codec.list Codec.string)
        |> Codec.field "re_inf" .re_inf (Codec.list entityCodec)
        |> Codec.field "re_pri" .re_pri (Codec.list Codec.string)
        |> Codec.buildObject


senseCodec : Codec Sense
senseCodec =
    Codec.object Sense
        |> Codec.field "stagk" .stagk (Codec.list Codec.string)
        |> Codec.field "stagr" .stagr (Codec.list Codec.string)
        |> Codec.field "pos" .pos (Codec.list entityCodec)
        |> Codec.field "xref" .xref (Codec.list Codec.string)
        |> Codec.field "ant" .ant (Codec.list Codec.string)
        |> Codec.field "field" .field (Codec.list entityCodec)
        |> Codec.field "misc" .misc (Codec.list entityCodec)
        |> Codec.field "s_inf" .s_inf (Codec.list Codec.string)
        |> Codec.field "lsource" .lsource (Codec.list lSourceCodec)
        |> Codec.field "dial" .dial (Codec.list entityCodec)
        |> Codec.field "gloss" .gloss (Codec.list glossCodec)
        |> Codec.field "example" .example (Codec.list exampleCodec)
        |> Codec.buildObject


lSourceCodec : Codec LSource
lSourceCodec =
    let
        attrCodec =
            Codec.object
                (\xmlLang ls_type ls_wasei ->
                    { xmlLang = xmlLang
                    , ls_type = ls_type
                    , ls_wasei = ls_wasei
                    }
                )
                |> Codec.nullableField "xmlLang" .xmlLang Codec.string
                |> Codec.nullableField "ls_type" .ls_type Codec.string
                |> Codec.field "ls_wasei" .ls_wasei Codec.bool
                |> Codec.buildObject
    in
    Codec.custom
        (\flSource value ->
            case value of
                LSource s a ->
                    flSource s a
        )
        |> Codec.variant2 "LSource" LSource Codec.string attrCodec
        |> Codec.buildCustom


glossCodec : Codec Gloss
glossCodec =
    let
        attrCodec =
            Codec.object
                (\xmlLang g_gend g_type ->
                    { xmlLang = xmlLang
                    , g_gend = g_gend
                    , g_type = g_type
                    }
                )
                |> Codec.nullableField "xmlLang" .xmlLang Codec.string
                |> Codec.nullableField "g_gend" .g_gend Codec.string
                |> Codec.nullableField "g_type" .g_type Codec.string
                |> Codec.buildObject
    in
    Codec.custom
        (\fGloss value ->
            case value of
                Gloss s a ->
                    fGloss s a
        )
        |> Codec.variant2 "Gloss" Gloss Codec.string attrCodec
        |> Codec.buildCustom


exampleCodec : Codec Example
exampleCodec =
    Codec.object
        (\ex_srce ex_text ex_sent ->
            { ex_srce = ex_srce
            , ex_text = ex_text
            , ex_sent = ex_sent
            }
        )
        |> Codec.field "ex_srce" .ex_srce Codec.string
        |> Codec.field "ex_text" .ex_text Codec.string
        |> Codec.field "ex_sent" .ex_sent (Codec.list ex_sentCodec)
        |> Codec.buildObject


ex_sentCodec : Codec Ex_sent
ex_sentCodec =
    let
        attrCodec =
            Codec.object
                (\xmlLang ex_srce ->
                    { xmlLang = xmlLang
                    , ex_srce = ex_srce
                    }
                )
                |> Codec.nullableField "xmlLang" .xmlLang Codec.string
                |> Codec.nullableField "ex_srce" .ex_srce Codec.string
                |> Codec.buildObject
    in
    Codec.custom
        (\fEx_sent value ->
            case value of
                Ex_sent s a ->
                    fEx_sent s a
        )
        |> Codec.variant2 "Ex_sent" Ex_sent Codec.string attrCodec
        |> Codec.buildCustom


entityCodec : Codec String
entityCodec =
    Codec.map entityDecoder entityEncoder Codec.string


entityEncoder : String -> String
entityEncoder content =
    case Dict.get content revEntityDict of
        Just { ref } ->
            "&" ++ ref ++ ";"

        Nothing ->
            content


entityDecoder : String -> String
entityDecoder mbRef =
    if String.startsWith "&" mbRef && String.endsWith ";" mbRef then
        let
            ref =
                String.dropLeft 1 mbRef
                    |> String.dropRight 1
        in
        case Dict.get ref entityDict of
            Just entity ->
                entity.content

            Nothing ->
                mbRef

    else
        mbRef



-------------------------------------------------------------------------------


entities =
    """<!ENTITY bra "Brazilian">
<!ENTITY hob "Hokkaido-ben">
<!ENTITY ksb "Kansai-ben">
<!ENTITY ktb "Kantou-ben">
<!ENTITY kyb "Kyoto-ben">
<!ENTITY kyu "Kyuushuu-ben">
<!ENTITY nab "Nagano-ben">
<!ENTITY osb "Osaka-ben">
<!ENTITY rkb "Ryuukyuu-ben">
<!ENTITY thb "Touhoku-ben">
<!ENTITY tsb "Tosa-ben">
<!ENTITY tsug "Tsugaru-ben">
<!-- <field> entities -->
<!ENTITY agric "agriculture">
<!ENTITY anat "anatomie">
<!ENTITY archeol "archéologie">
<!ENTITY archit "architecture">
<!ENTITY art "art, esthétique">
<!ENTITY astron "astronomie">
<!ENTITY audvid "audiovisuel">
<!ENTITY aviat "aviation">
<!ENTITY baseb "baseball">
<!ENTITY biochem "biochimie">
<!ENTITY biol "biologie">
<!ENTITY bot "botanique">
<!ENTITY Buddh "bouddhisme">
<!ENTITY bus "business">
<!ENTITY chem "chimie">
<!ENTITY Christn "Christianisme">
<!ENTITY cloth "vêtements">
<!ENTITY comp "informatique">
<!ENTITY cryst "cristallographie">
<!ENTITY ecol "écologie">
<!ENTITY econ "économie">
<!ENTITY elec "électricité">
<!ENTITY electr "électronique">
<!ENTITY embryo "embryologie">
<!ENTITY engr "ingénierie">
<!ENTITY ent "entomologie">
<!ENTITY finc "finance">
<!ENTITY fish "pêche">
<!ENTITY food "alimentation, cuisine">
<!ENTITY gardn "jardinage, horticulture">
<!ENTITY genet "génétique">
<!ENTITY geogr "géographie">
<!ENTITY geol "géologie">
<!ENTITY geom "géométrie">
<!ENTITY go "go (jeu)">
<!ENTITY golf "golf">
<!ENTITY gramm "grammaire">
<!ENTITY grmyth "mythologie grecque">
<!ENTITY hanaf "hanafuda">
<!ENTITY horse "course équestre">
<!ENTITY law "droit">
<!ENTITY ling "linguistique">
<!ENTITY logic "logique">
<!ENTITY MA "arts martiaux">
<!ENTITY mahj "mahjong">
<!ENTITY math "mathématique">
<!ENTITY mech "ingénierie mécanique">
<!ENTITY med "médecine">
<!ENTITY met "météorologie">
<!ENTITY mil "militaire">
<!ENTITY music "musique">
<!ENTITY ornith "ornithologie">
<!ENTITY paleo "paléontologie">
<!ENTITY pathol "pathologie">
<!ENTITY pharm "pharmacie">
<!ENTITY phil "philosophie">
<!ENTITY photo "photographie">
<!ENTITY physics "physique">
<!ENTITY physiol "physiologie">
<!ENTITY print "impression">
<!ENTITY psy "psychiatrie">
<!ENTITY psych "psychologie">
<!ENTITY rail "voie ferrée">
<!ENTITY Shinto "Shinto">
<!ENTITY shogi "shogi">
<!ENTITY sports "sports">
<!ENTITY stat "statistiques">
<!ENTITY sumo "sumo">
<!ENTITY telec "télécommunications">
<!ENTITY tradem "marque réservée">
<!ENTITY vidg "jeu vidéo">
<!ENTITY zool "zoologie">
<!-- <ke_inf> (kanji info) entities -->
<!ENTITY ateji "prononciation ateji (phonétique)">
<!ENTITY ik "Mot avec un usage irrégulier des kana">
<!ENTITY iK "Mot avec un usage irrégulier des kanji">
<!ENTITY io "usage irrégulier des okurigana">
<!ENTITY oK "kanji démodés ou usage désuet">
<!ENTITY rK "forme de kanji rare">
<!-- <misc> (miscellaneous) entities -->
<!ENTITY abbr "abréviation">
<!ENTITY arch "archaïsme">
<!ENTITY char "caractère">
<!ENTITY chn "langage enfantin">
<!ENTITY col "expression familière">
<!ENTITY company "nom d'entreprise">
<!ENTITY creat "créature">
<!ENTITY dated "terme daté">
<!ENTITY dei "divinité">
<!ENTITY derog "dérogatoire">
<!ENTITY doc "document">
<!ENTITY ev "événement">
<!ENTITY fam "langage familier">
<!ENTITY fem "terme ou expression féminine">
<!ENTITY fict "fiction">
<!ENTITY form "formel or littéraire">
<!ENTITY given "prénom, genre non spécifié">
<!ENTITY group "groupe">
<!ENTITY hist "terme historique">
<!ENTITY hon "langage honorifique or respectueux (sonkeigo)">
<!ENTITY hum "langage humble (kenjougo)">
<!ENTITY id "expression idiomatique">
<!ENTITY joc "humoristique">
<!ENTITY leg "légende">
<!ENTITY m-sl "argot manga">
<!ENTITY male "terme ou expression masculine">
<!ENTITY myth "mythologie">
<!ENTITY net-sl "argot internet">
<!ENTITY obj "objet">
<!ENTITY obs "terme dépassé">
<!ENTITY obsc "terme abscons">
<!ENTITY on-mim "onomatopée ou mimétique">
<!ENTITY organization "nom d'une organisation">
<!ENTITY oth "autre">
<!ENTITY person "nom de personne (complet)">
<!ENTITY place "nom de lieu">
<!ENTITY poet "terme poétique">
<!ENTITY pol "langage poli (teineigo)">
<!ENTITY product "nom de produit">
<!ENTITY proverb "proverbe">
<!ENTITY quote "citation">
<!ENTITY rare "rare">
<!ENTITY relig "religieux">
<!ENTITY sens "sensible">
<!ENTITY serv "service">
<!ENTITY sl "argot">
<!ENTITY station "gare">
<!ENTITY surname "nom de famille">
<!ENTITY uk "mot habituellement écrit en kana">
<!ENTITY unclass "nom non-classé">
<!ENTITY vulg "vulgaire">
<!ENTITY work "œuvre d'art">
<!ENTITY X "vulgaire">
<!ENTITY yoji "yojijukugo">
<!-- <pos> (part-of-speech) entities -->
<!ENTITY adj-f "nom ou verbe agissant prénominalement">
<!ENTITY adj-i "adjectif (keiyoushi)">
<!ENTITY adj-ix "adjectif (keiyoushi) - classe yoi/ii">
<!ENTITY adj-kari "adjectif en 'kari' (archaïque)">
<!ENTITY adj-ku "adjectif en 'ku' (archaïque)">
<!ENTITY adj-na "adjectif en 'na' (keiyodoshi)">
<!ENTITY adj-nari "adjectif en 'na' utilisant la forme archaïque/formel 'nari'">
<!ENTITY adj-no "adjectif en 'no'">
<!ENTITY adj-pn "adjectifs pré-nominaux (rentaishi)">
<!ENTITY adj-shiku "adjectif en 'shiku' (archaïque)">
<!ENTITY adj-t "adjectif en 'taru'">
<!ENTITY adv "adverbe (fukushi)">
<!ENTITY adv-to "adverbe qui utilise la particule 'to'">
<!ENTITY aux "auxiliaire">
<!ENTITY aux-adj "adjectif auxiliaire">
<!ENTITY aux-v "verbe auxiliaire">
<!ENTITY conj "conjonction">
<!ENTITY cop "copule">
<!ENTITY ctr "compteur">
<!ENTITY exp "expressions (phrases, clauses, etc.)">
<!ENTITY int "interjection (kandoushi)">
<!ENTITY n "nom (commun) (futsuumeishi)">
<!ENTITY n-adv "nom adverbial (fukushitekimeishi)">
<!ENTITY n-pr "nom propre">
<!ENTITY n-pref "nom utilisé comme préfixe">
<!ENTITY n-suf "nom utilisé comme suffixe">
<!ENTITY n-t "nom (temporel) (jisoumeishi)">
<!ENTITY num "numérique">
<!ENTITY pn "pronom">
<!ENTITY pref "préfixe">
<!ENTITY prt "particule">
<!ENTITY suf "suffixe">
<!ENTITY unc "non-classé">
<!ENTITY v-unspec "verbe non spécifié">
<!ENTITY v1 "verbe Ichidan">
<!ENTITY v1-s "verbe Ichidan - groupe kureru">
<!ENTITY v2a-s "verbe Nidan qui se finit par 'u' (archaïque)">
<!ENTITY v2b-k "verbe Kami-nidan qui se finit par 'bu' (archaïque)">
<!ENTITY v2b-s "verbe Shimo-nidan qui se finit par 'bu' (archaïque)">
<!ENTITY v2d-k "verbe Kami-nidan qui se finit par 'dzu' (archaïque)">
<!ENTITY v2d-s "verbe Shimo-nidan qui se finit par 'dzu' (archaïque)">
<!ENTITY v2g-k "verbe Kami-nidan qui se finit par 'gu' (archaïque)">
<!ENTITY v2g-s "verbe Shimo-nidan qui se finit par 'gu' (archaïque)">
<!ENTITY v2h-k "verbe Kami-nidan qui se finit par 'bu' (archaïque)">
<!ENTITY v2h-s "verbe Shimo-nidan qui se finit par 'hu/fu' (archaïque)">
<!ENTITY v2k-k "verbe Kami-nidan qui se finit par 'ku' (archaïque)">
<!ENTITY v2k-s "verbe Shimo-nidan qui se finit par 'ku' (archaïque)">
<!ENTITY v2m-k "verbe Kami-nidan qui se finit par 'mu' (archaïque)">
<!ENTITY v2m-s "verbe Shimo-nidan qui se finit par 'mu' (archaïque)">
<!ENTITY v2n-s "verbe Shimo-nidan qui se finit par 'nu' (archaïque)">
<!ENTITY v2r-k "verbe Kami-nidan qui se finit par 'ru' (archaïque)">
<!ENTITY v2r-s "verbe Shimo-nidan qui se finit par 'ru' (archaïque)">
<!ENTITY v2s-s "verbe Shimo-nidan qui se finit par 'su' (archaïque)">
<!ENTITY v2t-k "verbe Kami-nidan qui se finit par 'tsu' (archaïque)">
<!ENTITY v2t-s "verbe Shimo-nidan qui se finit par 'tsu' (archaïque)">
<!ENTITY v2w-s "verbe Shimo-nidan qui se finit par 'u' avec une conjugaison en 'we' (archaïque)">
<!ENTITY v2y-k "verbe Kami-nidan qui se finit par 'yu' (archaïque)">
<!ENTITY v2y-s "verbe Shimo-nidan qui se finit par 'yu' (archaïque)">
<!ENTITY v2z-s "verbe Shimo-nidan qui se finit par 'zu' (archaïque)">
<!ENTITY v4b "verbe Yodan qui se finit en 'bu' (archaïque)">
<!ENTITY v4g "verbe Yodan qui se finit en 'gu' (archaïque)">
<!ENTITY v4h "verbe Yodan qui se finit en 'hu/fu' (archaïque)">
<!ENTITY v4k "verbe Yodan qui se finit en 'ku' (archaïque)">
<!ENTITY v4m "verbe Yodan qui se finit en 'mu' (archaïque)">
<!ENTITY v4n "verbe Yodan qui se finit en 'nu' (archaïque)">
<!ENTITY v4r "verbe Yodan qui se finit en 'ru' (archaïque)">
<!ENTITY v4s "verbe Yodan qui se finit en 'su' (archaïque)">
<!ENTITY v4t "verbe Yodan qui se finit en 'tsu' (archaïque)">
<!ENTITY v5aru "verbe Godan, groupe -aru">
<!ENTITY v5b "verbe Godan qui se finit en 'bu'">
<!ENTITY v5g "verbe Godan qui se finit en 'gu'">
<!ENTITY v5k "verbe Godan qui se finit en 'ku'">
<!ENTITY v5k-s "verbe Godan, groupe -iku/yuku">
<!ENTITY v5m "verbe Godan qui se finit en 'mu'">
<!ENTITY v5n "verbe Godan qui se finit en 'nu'">
<!ENTITY v5r "verbe Godan qui se finit en 'ru'">
<!ENTITY v5r-i "verbe Godan qui se finit en 'ru' (irrégulier)">
<!ENTITY v5s "verbe Godan qui se finit en 'su'">
<!ENTITY v5t "verbe Godan qui se finit en 'tsu'">
<!ENTITY v5u "verbe Godan qui se finit en 'u'">
<!ENTITY v5u-s "verbe Godan qui se finit en 'u' (cas particulier)">
<!ENTITY v5uru "verbe Godan- ancien groupe en 'Uru' (mod. 'Eru')">
<!ENTITY vi "verbe intransitif">
<!ENTITY vk "verbe Kuru - cas particulier">
<!ENTITY vn "verbe irrégulier en 'nu'">
<!ENTITY vr "verbe irrégulier en 'ru', la forme simple se termine par -ri">
<!ENTITY vs "nom ou participe qui se combine avec suru">
<!ENTITY vs-c "verbe su (archaïque) - précurseur de suru">
<!ENTITY vs-i "verbe suru - inclus">
<!ENTITY vs-s "verbe suru - cas particulier">
<!ENTITY vt "verbe transitif">
<!ENTITY vz "verbe Ichidan - en zuru (forme alternative aux verbes en -jiru)">
<!-- <re_inf> (reading info) entities -->
<!ENTITY gikun "prononciation irrégulière gikun ou jukujikun">
<!ENTITY ik "usage irrégulier des kana">
<!ENTITY ok "usage des kana dépassé">
<!ENTITY uK "mot habituellement écrit en kanji">"""


entityDict =
    String.lines entities
        |> List.indexedMap (\n -> Parser.run (entityParser n))
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.map (\e -> ( e.ref, e ))
        |> Dict.fromList


revEntityDict =
    Dict.toList entityDict
        |> List.map (\( ref, e ) -> ( e.content, e ))
        |> Dict.fromList


type alias Entity =
    { id : Int
    , ref : String
    , content : String
    }


entityParser : Int -> Parser Entity
entityParser n =
    Parser.succeed (Entity n)
        |. symbol "<!ENTITY"
        |. spaces
        |= (chompWhile (\c -> Char.isAlphaNum c || c /= ' ')
                |> getChompedString
           )
        |. spaces
        |. symbol "\""
        |= (chompWhile (\c -> c /= '"')
                |> getChompedString
           )
        |. symbol "\">"


entitiesPreProcessor xml =
    String.lines xml
