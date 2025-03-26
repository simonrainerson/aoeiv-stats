module GamesDecoder exposing (Game, MatchResult(..), Player, Team, fullTeam, gameDecoder, gamesDecoder)

import Civilizations exposing (..)
import Json.Decode exposing (Decoder, andThen, at, fail, field, index, int, list, map6, map7, nullable, string, succeed)



--- Player IDs


simon : number
simon =
    1286005


daniel : number
daniel =
    9821030


linus : number
linus =
    9899004



--- Internal Types


type alias ParsedPlayer =
    { id : Int
    , name : String
    , country : String
    , mmr : Maybe Int
    , civ : Civilization
    , result : MatchResult
    }


type alias ParsedGame =
    { gameId : Int
    , started : String
    , duration_secs : Maybe Int
    , map : String
    , team1 : List ParsedPlayer
    , team2 : List ParsedPlayer
    , season : Int
    }



--- Public Types


type alias Player =
    { id : Int
    , name : String
    , country : String
    , civ : Civilization
    , mmr : Maybe Int
    }


type alias Team =
    List Player


type alias Game =
    { gameId : Int
    , started : String
    , duration_secs : Maybe Int
    , map : String
    , result : MatchResult
    , ourTeam : Team
    , enemyTeam : Team
    , season : Int
    }


type MatchResult
    = Win
    | Loss
    | Unknown



--- Decoders


gameDecoder : Decoder Game
gameDecoder =
    map7 ParsedGame
        (field "game_id" int)
        (field "started_at" string)
        (field "duration" (nullable int))
        (field "map" string)
        (field "teams" (index 0 (list playerDecoder)))
        (field "teams" (index 1 (list playerDecoder)))
        (field "season" int)
        |> andThen toGame


toGame : ParsedGame -> Decoder Game
toGame parsedGame =
    let
        ( heroes, foes ) =
            if List.any (\x -> List.member x.id [ simon, daniel, linus ]) parsedGame.team1 then
                ( parsedGame.team1, parsedGame.team2 )

            else
                ( parsedGame.team2, parsedGame.team1 )
    in
    succeed
        (Game
            parsedGame.gameId
            parsedGame.started
            parsedGame.duration_secs
            parsedGame.map
            (case heroes |> List.head of
                Just player ->
                    player |> .result

                _ ->
                    Unknown
            )
            (heroes
                |> List.map toPlayer
            )
            (foes
                |> List.map toPlayer
            )
            parsedGame.season
        )


toPlayer : ParsedPlayer -> Player
toPlayer parsedPlayer =
    Player
        parsedPlayer.id
        parsedPlayer.name
        parsedPlayer.country
        parsedPlayer.civ
        parsedPlayer.mmr


playerDecoder : Decoder ParsedPlayer
playerDecoder =
    map6 ParsedPlayer
        (at [ "player", "profile_id" ] int)
        (at [ "player", "name" ] string)
        (at [ "player", "country" ] string)
        (at [ "player", "mmr" ] (nullable int))
        (at [ "player", "civilization" ] string |> andThen civilizationDecoder)
        (at [ "player", "result" ] (nullable string |> andThen matchResultDecoder))


civilizationDecoder : String -> Decoder Civilization
civilizationDecoder civName =
    case civilizationFromString civName of
        Just civ ->
            succeed civ

        Nothing ->
            fail "Could not decode Civilization"


matchResultFromString : String -> MatchResult
matchResultFromString res =
    case res of
        "win" ->
            Win

        "loss" ->
            Loss

        _ ->
            Unknown


matchResultDecoder : Maybe String -> Decoder MatchResult
matchResultDecoder res =
    case res of
        Just result ->
            succeed (matchResultFromString result)

        _ ->
            succeed Unknown



--- Public Functions


fullTeam : Game -> Bool
fullTeam game =
    game.ourTeam |> List.map (\p -> p.id) |> List.all (\id -> List.member id [ simon, daniel, linus ])


gamesDecoder : Decoder (List Game)
gamesDecoder =
    field "games" (list gameDecoder)
