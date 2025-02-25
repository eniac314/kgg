module Helpers exposing (..)

import Http exposing (..)
import Random


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        BadUrl s ->
            "Url invalide: " ++ s

        Timeout ->
            "Délai d'attente dépassé"

        NetworkError ->
            "Erreur de réseau"

        BadStatus statusCode ->
            "Erreur serveur: "
                ++ String.fromInt statusCode

        BadBody details ->
            "Erreur décodage: " ++ details


shuffle : Random.Seed -> List a -> List a
shuffle seed xs =
    let
        n =
            List.length xs

        ( randlist, _ ) =
            Random.step (Random.list n (Random.float 0 1)) seed
    in
    List.map2 Tuple.pair randlist xs
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


shuffleSeed : Random.Seed -> List a -> ( Random.Seed, List a )
shuffleSeed seed xs =
    let
        n =
            List.length xs

        ( randlist, newSeed ) =
            Random.step (Random.list n (Random.float 0 1)) seed
    in
    ( newSeed
    , List.map2 Tuple.pair randlist xs
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
    )


mbStr s =
    if s == "" then
        Nothing

    else
        Just s


isHanzi c =
    let
        code =
            Char.toCode c
    in
    c == '々' || (code >= 0x4E00 && code <= 0x9FBF)


isKana c =
    let
        code =
            Char.toCode c
    in
    code >= 0x3040 && code <= 0x30FF
