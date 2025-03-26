module Decoding exposing (suite)

import Civilizations exposing (Civilization(..))
import Expect exposing (equal, fail)
import GamesDecoder exposing (Game, MatchResult(..), Player, gameDecoder, gamesDecoder)
import Json.Decode exposing (decodeString, errorToString)
import Test exposing (Test, describe, test)


singleGame : String
singleGame =
    """
{
  "game_id": 165415960,
  "started_at": "2025-02-11T20:20:06.000Z",
  "updated_at": "2025-02-11T21:50:13.111Z",
  "duration": 2910,
  "map": "Mongolian Heights",
  "kind": "rm_3v3",
  "leaderboard": "rm_team",
  "mmr_leaderboard": "rm_3v3",
  "season": 9,
  "server": "UK",
  "patch": 2638,
  "average_rating": 1139,
  "average_rating_deviation": 128,
  "average_mmr": 1137,
  "average_mmr_deviation": 145,
  "ongoing": false,
  "just_finished": false,
  "teams": [
    [
      {
        "player": {
          "profile_id": 1818587,
          "name": "Rick iZ Boss",
          "country": "nl",
          "result": "loss",
          "civilization": "rus",
          "civilization_randomized": false,
          "rating": 1066,
          "rating_diff": -21,
          "mmr": 956,
          "mmr_diff": -10,
          "input_type": "keyboard"
        }
      },
      {
        "player": {
          "profile_id": 5060490,
          "name": "BoerBont",
          "country": "nl",
          "result": "loss",
          "civilization": "delhi_sultanate",
          "civilization_randomized": false,
          "rating": 1086,
          "rating_diff": -16,
          "mmr": 1080,
          "mmr_diff": -10,
          "input_type": "keyboard"
        }
      },
      {
        "player": {
          "profile_id": 8670336,
          "name": "El Svennano",
          "country": "nl",
          "result": "loss",
          "civilization": "order_of_the_dragon",
          "civilization_randomized": false,
          "rating": 1259,
          "rating_diff": -16,
          "mmr": 1170,
          "mmr_diff": -10,
          "input_type": "keyboard"
        }
      }
    ],
    [
      {
        "player": {
          "profile_id": 9899004,
          "name": "Smedja",
          "country": "se",
          "result": "win",
          "civilization": "english",
          "civilization_randomized": false,
          "rating": 935,
          "rating_diff": 15,
          "mmr": 999,
          "mmr_diff": 10,
          "input_type": "keyboard"
        }
      },
      {
        "player": {
          "profile_id": 9821030,
          "name": "Farfar ⤜( ͠° ͜ʖ °)⤏",
          "country": "se",
          "result": "win",
          "civilization": "abbasid_dynasty",
          "civilization_randomized": false,
          "rating": 1324,
          "rating_diff": 15,
          "mmr": 1388,
          "mmr_diff": 10,
          "input_type": "keyboard"
        }
      },
      {
        "player": {
          "profile_id": 1286005,
          "name": "⚔️ 𝕾𝖎𝖒𝖔𝖓 ⚔",
          "country": "se",
          "result": "win",
          "civilization": "malians",
          "civilization_randomized": false,
          "rating": 1166,
          "rating_diff": 15,
          "mmr": 1226,
          "mmr_diff": 10,
          "input_type": "keyboard"
        }
      }
    ]
  ]
}
"""


fullRequest : String
fullRequest =
    """
{
  "total_count": 141,
  "page": 2,
  "per_page": 50,
  "count": 50,
  "offset": 50,
  "filters": {
    "leaderboard": "rm_3v3",
    "since": "2024-11-01T00:00:00.000Z",
    "profile_ids": [
      1286005
    ],
    "opponent_profile_id": null,
    "opponent_profile_ids": null
  },
  "games": [
    {
      "game_id": 165415960,
      "started_at": "2025-02-11T20:20:06.000Z",
      "updated_at": "2025-02-11T21:50:13.111Z",
      "duration": 2910,
      "map": "Mongolian Heights",
      "kind": "rm_3v3",
      "leaderboard": "rm_team",
      "mmr_leaderboard": "rm_3v3",
      "season": 9,
      "server": "UK",
      "patch": 2638,
      "average_rating": 1139,
      "average_rating_deviation": 128,
      "average_mmr": 1137,
      "average_mmr_deviation": 145,
      "ongoing": false,
      "just_finished": false,
      "teams": [
        [
          {
            "player": {
              "profile_id": 1818587,
              "name": "Rick iZ Boss",
              "country": "nl",
              "result": "loss",
              "civilization": "rus",
              "civilization_randomized": false,
              "rating": 1066,
              "rating_diff": -21,
              "mmr": 956,
              "mmr_diff": -10,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 5060490,
              "name": "BoerBont",
              "country": "nl",
              "result": "loss",
              "civilization": "delhi_sultanate",
              "civilization_randomized": false,
              "rating": 1086,
              "rating_diff": -16,
              "mmr": 1080,
              "mmr_diff": -10,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 8670336,
              "name": "El Svennano",
              "country": "nl",
              "result": "loss",
              "civilization": "order_of_the_dragon",
              "civilization_randomized": false,
              "rating": 1259,
              "rating_diff": -16,
              "mmr": 1170,
              "mmr_diff": -10,
              "input_type": "keyboard"
            }
          }
        ],
        [
          {
            "player": {
              "profile_id": 9899004,
              "name": "Smedja",
              "country": "se",
              "result": "win",
              "civilization": "english",
              "civilization_randomized": false,
              "rating": 935,
              "rating_diff": 15,
              "mmr": 999,
              "mmr_diff": 10,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 9821030,
              "name": "Farfar ⤜( ͠° ͜ʖ °)⤏",
              "country": "se",
              "result": "win",
              "civilization": "abbasid_dynasty",
              "civilization_randomized": false,
              "rating": 1324,
              "rating_diff": 15,
              "mmr": 1388,
              "mmr_diff": 10,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 1286005,
              "name": "⚔️ 𝕾𝖎𝖒𝖔𝖓 ⚔",
              "country": "se",
              "result": "win",
              "civilization": "malians",
              "civilization_randomized": false,
              "rating": 1166,
              "rating_diff": 15,
              "mmr": 1226,
              "mmr_diff": 10,
              "input_type": "keyboard"
            }
          }
        ]
      ]
    },
    {
      "game_id": 165411569,
      "started_at": "2025-02-11T19:42:26.000Z",
      "updated_at": "2025-02-11T22:02:17.718Z",
      "duration": 2149,
      "map": "Hideout",
      "kind": "rm_3v3",
      "leaderboard": "rm_team",
      "mmr_leaderboard": "rm_3v3",
      "season": 9,
      "server": "Europe (W)",
      "patch": 2638,
      "average_rating": 1109,
      "average_rating_deviation": 199,
      "average_mmr": 1207,
      "average_mmr_deviation": null,
      "ongoing": false,
      "just_finished": false,
      "teams": [
        [
          {
            "player": {
              "profile_id": 20888704,
              "name": "a1ryaaa5032",
              "country": "us",
              "result": "win",
              "civilization": "mongols",
              "civilization_randomized": false,
              "rating": 755,
              "rating_diff": 42,
              "mmr": null,
              "mmr_diff": null,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 21258964,
              "name": "Lord kivi",
              "country": "ua",
              "result": "win",
              "civilization": "rus",
              "civilization_randomized": false,
              "rating": 1271,
              "rating_diff": 28,
              "mmr": 1212,
              "mmr_diff": 19,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 1789276,
              "name": "nicholasschneider",
              "country": "de",
              "result": "win",
              "civilization": "english",
              "civilization_randomized": false,
              "rating": 1118,
              "rating_diff": 28,
              "mmr": 1153,
              "mmr_diff": 19,
              "input_type": "keyboard"
            }
          }
        ],
        [
          {
            "player": {
              "profile_id": 9821030,
              "name": "Farfar ⤜( ͠° ͜ʖ °)⤏",
              "country": "se",
              "result": "loss",
              "civilization": "abbasid_dynasty",
              "civilization_randomized": false,
              "rating": 1353,
              "rating_diff": -29,
              "mmr": 1407,
              "mmr_diff": -19,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 9899004,
              "name": "Smedja",
              "country": "se",
              "result": "loss",
              "civilization": "english",
              "civilization_randomized": false,
              "rating": 964,
              "rating_diff": -29,
              "mmr": 1018,
              "mmr_diff": -19,
              "input_type": "keyboard"
            }
          },
          {
            "player": {
              "profile_id": 1286005,
              "name": "⚔️ 𝕾𝖎𝖒𝖔𝖓 ⚔",
              "country": "se",
              "result": "loss",
              "civilization": "holy_roman_empire",
              "civilization_randomized": false,
              "rating": 1195,
              "rating_diff": -29,
              "mmr": 1245,
              "mmr_diff": -19,
              "input_type": "keyboard"
            }
          }
        ]
      ]
    }
  ]
}
    """


game1 : Game
game1 =
    Game
        165415960
        "2025-02-11T20:20:06.000Z"
        (Just 2910)
        "Mongolian Heights"
        Win
        [ Player
            9899004
            "Smedja"
            "se"
            English
            (Just 999)
        , Player
            9821030
            "Farfar ⤜( ͠° ͜ʖ °)⤏"
            "se"
            AbbasidDynasty
            (Just 1388)
        , Player
            1286005
            "⚔️ 𝕾𝖎𝖒𝖔𝖓 ⚔"
            "se"
            Malians
            (Just 1226)
        ]
        [ Player
            1818587
            "Rick iZ Boss"
            "nl"
            Rus
            (Just 956)
        , Player
            5060490
            "BoerBont"
            "nl"
            DelhiSultanate
            (Just 1080)
        , Player
            8670336
            "El Svennano"
            "nl"
            OrderOfTheDragon
            (Just 1170)
        ]
        9


game2 : Game
game2 =
    Game
        165411569
        "2025-02-11T19:42:26.000Z"
        (Just 2149)
        "Hideout"
        Loss
        [ Player
            9821030
            "Farfar ⤜( ͠° ͜ʖ °)⤏"
            "se"
            AbbasidDynasty
            (Just 1407)
        , Player
            9899004
            "Smedja"
            "se"
            English
            (Just 1018)
        , Player
            1286005
            "⚔️ 𝕾𝖎𝖒𝖔𝖓 ⚔"
            "se"
            HolyRomanEmpire
            (Just 1245)
        ]
        [ Player
            20888704
            "a1ryaaa5032"
            "us"
            Mongols
            Nothing
        , Player
            21258964
            "Lord kivi"
            "ua"
            Rus
            (Just 1212)
        , Player
            1789276
            "nicholasschneider"
            "de"
            English
            (Just 1153)
        ]
        9


suite : Test
suite =
    describe "Test decoding a resonse"
        [ test "Decode a single game" <|
            \_ ->
                case decodeString gameDecoder singleGame of
                    Ok res ->
                        res |> equal game1

                    Err error ->
                        error |> errorToString |> fail
        , test "Decode a list of games" <|
            \_ ->
                case decodeString gamesDecoder fullRequest of
                    Ok res ->
                        res |> equal [ game1, game2 ]

                    Err error ->
                        error |> errorToString |> fail
        ]
