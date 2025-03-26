module Filter exposing (suite)

import Civilizations exposing (Civilization(..))
import Expect exposing (equal)
import GamesDecoder exposing (Game, MatchResult(..), Player)
import Main exposing (AllFilters, CivilizationFilter, Side(..), applyCivFilter, applyFilters)
import Test exposing (Test, describe, test)


anyEnemy : CivilizationFilter
anyEnemy =
    CivilizationFilter Enemy List.any []


anyCivBoth : AllFilters
anyCivBoth =
    AllFilters
        (CivilizationFilter Hero List.any [])
        (CivilizationFilter Enemy List.all [])


andFrVsAny : AllFilters
andFrVsAny =
    AllFilters
        (CivilizationFilter Hero List.all [ French ])
        (CivilizationFilter Enemy List.all [])


andFrAbbaVsAny : AllFilters
andFrAbbaVsAny =
    AllFilters
        (CivilizationFilter Hero List.all [ French, AbbasidDynasty ])
        anyEnemy


andFrAbbaVsOrOtto : AllFilters
andFrAbbaVsOrOtto =
    AllFilters
        (CivilizationFilter Hero List.all [ French, AbbasidDynasty ])
        (CivilizationFilter Enemy List.any [ Ottomans ])


andFrAbbaVsOrDelhiOtto : AllFilters
andFrAbbaVsOrDelhiOtto =
    AllFilters
        (CivilizationFilter Hero List.all [ French, AbbasidDynasty ])
        (CivilizationFilter Enemy List.any [ DelhiSultanate, Ottomans ])


andFrAbbaVsOrDelhiJeanEng : AllFilters
andFrAbbaVsOrDelhiJeanEng =
    AllFilters
        (CivilizationFilter Hero List.all [ French, AbbasidDynasty ])
        (CivilizationFilter Enemy List.any [ DelhiSultanate, JeanneDarc, English ])


andFrAbbaVsOrFive : AllFilters
andFrAbbaVsOrFive =
    AllFilters
        (CivilizationFilter Hero List.all [ French, AbbasidDynasty ])
        (CivilizationFilter Enemy List.any [ DelhiSultanate, Ottomans, English, Ayyubids, JeanneDarc ])


andEngAbbaVsAny : AllFilters
andEngAbbaVsAny =
    AllFilters
        (CivilizationFilter Hero List.all [ English, AbbasidDynasty ])
        anyEnemy


frAbbaMong : Game
frAbbaMong =
    Game
        0
        "1970-01-01"
        Nothing
        "Dry Arabia"
        Win
        [ Player 1 "" "" French (Just 1)
        , Player 2 "" "" AbbasidDynasty (Just 1)
        , Player 3 "" "" Mongols (Just 1)
        ]
        [ Player 4 "" "" French (Just 1)
        , Player 5 "" "" Ottomans (Just 1)
        , Player 6 "" "" Byzantines (Just 1)
        ]
        0


suite : Test
suite =
    describe "Test filtering games"
        [ describe "Test applyCivFilter, positive."
            [ test "Test any filter for hero civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Hero List.all []) frAbbaMong) True
            , test "Test any filter for enemy civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Enemy List.all []) frAbbaMong) True
            , test "Test or filter for hero civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Hero List.any [ French, Ottomans ]) frAbbaMong) True
            , test "Test and filter for hero civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Hero List.all [ French, AbbasidDynasty ]) frAbbaMong) True
            , test "Test or filter for enemy civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Enemy List.any [ French, DelhiSultanate ]) frAbbaMong) True
            , test "Test and filter for enemy civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Enemy List.all [ French, Byzantines ]) frAbbaMong) True
            ]
        , describe "Test applyCivFilter, negative."
            [ test "Test or filter for hero civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Hero List.any [ Ayyubids, Ottomans, Byzantines ]) frAbbaMong) False
            , test "Test and filter for hero civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Hero List.all [ French, AbbasidDynasty, English ]) frAbbaMong) False
            , test "Test or filter for enemy civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Enemy List.any [ Mongols, DelhiSultanate ]) frAbbaMong) False
            , test "Test and filter for enemy civs" <|
                \_ -> equal (applyCivFilter (CivilizationFilter Enemy List.all [ French, AbbasidDynasty ]) frAbbaMong) False
            ]
        , describe "Test applyAllFilters"
            [ test "Filter using the any filter for both teams." <|
                \_ ->
                    equal (applyFilters anyCivBoth frAbbaMong) True
            , test "Filter using the and filter for with just one civ." <|
                \_ ->
                    equal (applyFilters andFrVsAny frAbbaMong) True
            , test "Filter using the and filter for with two civs." <|
                \_ ->
                    equal (applyFilters andFrAbbaVsAny frAbbaMong) True
            , test "Filter using the and filter for with two civs vs one." <|
                \_ ->
                    equal (applyFilters andFrAbbaVsOrOtto frAbbaMong) True
            , test "Filter using the and filter for with two civs vs two." <|
                \_ ->
                    equal (applyFilters andFrAbbaVsOrDelhiOtto frAbbaMong) True
            , test "Filter using the and filter for with two civs vs five." <|
                \_ ->
                    equal (applyFilters andFrAbbaVsOrFive frAbbaMong) True
            , test "Filter using the and filter for with two (one wrong) civs vs any." <|
                \_ ->
                    equal (applyFilters andEngAbbaVsAny frAbbaMong) False
            , test "Filter using the and filter for with two civs vs many non matching." <|
                \_ ->
                    equal (applyFilters andFrAbbaVsOrDelhiJeanEng frAbbaMong) False
            ]
        ]
