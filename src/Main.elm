module Main exposing (..)

import Browser
import Civilizations exposing (Civilization(..), civilizationToString)
import Debug
import Dict
import GamesDecoder exposing (..)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Http



-- STUFF


type alias GameDict =
    Dict.Dict String (List Game)


type Msg
    = GotGames (Result Http.Error (List Game))
    | Toggle Side Civilization


wins : List Game -> Int
wins games =
    games |> List.map (\game -> game |> .result) |> List.filter (\result -> result == Win) |> List.length


losses : List Game -> Int
losses games =
    games |> List.map (\game -> game |> .result) |> List.filter (\result -> result == Loss) |> List.length


getGames : Int -> Cmd Msg
getGames page =
    Http.get
        { url = "https://aoe4world.com/api/v0/players/76561197985789866/games?leaderboard=rm_3v3&since=2024-10-01&page=" ++ String.fromInt page
        , expect = Http.expectJson GotGames gamesDecoder
        }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias CivilizationFilter =
    { side : Side
    , combiner : (Civilization -> Bool) -> List Civilization -> Bool
    , civs : List Civilization
    }


type alias AllFilters =
    { heroCivs : CivilizationFilter
    , enemyCivs : CivilizationFilter
    }


type Model
    = Failure
    | Loading
    | Success ( List Game, AllFilters )


type Side
    = Hero
    | Enemy


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Cmd.batch
        [ getGames 1
        , getGames 2
        , getGames 3
        , getGames 4
        ]
    )



-- UPDATE


gameFilter : Game -> Bool
gameFilter game =
    game.result
        /= Unknown
        && fullTeam game
        && game.season
        == 9


validGames : List Game -> List Game
validGames games =
    games |> List.filter gameFilter


extendGames : Game -> GameDict -> GameDict
extendGames game gameDict =
    Dict.update game.map
        (\v ->
            case v of
                Just gameList ->
                    Just (game :: gameList)

                Nothing ->
                    Just [ game ]
        )
        gameDict


applyCivFilter : CivilizationFilter -> Game -> Bool
applyCivFilter filter game =
    (if filter.side == Hero then
        game.ourTeam

     else
        game.enemyTeam
    )
        |> List.map (\p -> p.civ)
        |> (\l -> filter.civs == [] || filter.combiner (\c -> List.member c l) filter.civs)


applyFilters : AllFilters -> Game -> Bool
applyFilters filters game =
    List.all
        (\fb -> fb)
        [ applyCivFilter filters.heroCivs game
        , applyCivFilter filters.enemyCivs game
        ]


mapMaps : List Game -> AllFilters -> GameDict
mapMaps games filters =
    List.foldl
        extendGames
        Dict.empty
        (List.filter (applyFilters filters) games)


toggleListMember : a -> List a -> List a
toggleListMember item list =
    if List.member item list then
        List.filter (\i -> i /= item) list

    else
        item :: list


toggleCiv : Civilization -> CivilizationFilter -> CivilizationFilter
toggleCiv civ filter =
    { filter | civs = toggleListMember civ filter.civs }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( old_games, filters ) =
            case model of
                Success ( last_games, last_filters ) ->
                    ( last_games, last_filters )

                _ ->
                    ( []
                    , AllFilters (CivilizationFilter Hero List.all []) (CivilizationFilter Enemy List.any [])
                    )
    in
    case Debug.log "update" msg of
        GotGames result ->
            case result of
                Ok games ->
                    ( Success ( old_games ++ (games |> validGames), filters )
                    , Cmd.none
                      -- Cmd.none -> Get next page if expected
                    )

                Err _ ->
                    Debug.log "Error" ( Failure, Cmd.none )

        Toggle side civ ->
            case model of
                Success ( games, filters_ ) ->
                    ( Success
                        ( games
                        , case side of
                            Hero ->
                                { filters_ | heroCivs = toggleCiv civ filters_.heroCivs }

                            Enemy ->
                                { filters_ | enemyCivs = toggleCiv civ filters_.enemyCivs }
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


statBox : String -> Int -> Int -> Html Msg
statBox name numWins totalGames =
    let
        winRate =
            100 * (numWins |> toFloat) / (totalGames |> toFloat)
    in
    div [ style "background-color" "#FAEBD7", style "margin" "1em", style "padding" "1em" ]
        [ div
            []
            [ text (name ++ ": ")
            , text ((numWins |> String.fromInt) ++ " wins out of " ++ (totalGames |> String.fromInt) ++ " games.")
            ]
        , div [ style "background-color" "darkred" ]
            [ div
                [ style "background-color" "green", style "width" (String.append (winRate |> String.fromFloat) "%") ]
                [ String.append (winRate |> String.fromFloat) "%" |> text ]
            ]
        ]


civbox : Civilization -> Side -> Bool -> Html Msg
civbox civ side on =
    div
        []
        [ input
            [ type_ "checkbox"
            , onClick ((\_ -> Toggle side civ) civ)
            , checked on
            ]
            []
        , civ |> civilizationToString |> text
        ]


civFilterBox : String -> CivilizationFilter -> Html Msg
civFilterBox title filters =
    let
        color =
            if filters.side == Hero then
                "lightGreen"

            else
                "pink"

        box =
            \c -> civbox c filters.side (List.member c filters.civs)
    in
    div
        [ style "border" "0.2em solid black"
        , style "border-radius" "0.5em"
        , style "padding" "0.2em"
        , style "margin" "0.2em"
        , style "background-color" color
        , style "width" "15em"
        ]
        [ text title
        , box AbbasidDynasty
        , box Ayyubids
        , box Byzantines
        , box Chinese
        , box DelhiSultanate
        , box English
        , box French
        , box HolyRomanEmpire
        , box Japanese
        , box JeanneDarc
        , box Malians
        , box Mongols
        , box OrderOfTheDragon
        , box Ottomans
        , box Rus
        , box ZhuXisLegacy
        ]


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Unable to load games from aoe4world."

        Loading ->
            text "Loading..."

        Success ( games, filters ) ->
            let
                gamesPerMap =
                    mapMaps games filters

                totalGames =
                    List.filter (applyFilters filters) games
            in
            div []
                (statBox
                    "Total"
                    (totalGames |> wins)
                    (totalGames |> List.length)
                    :: (gamesPerMap |> Dict.toList |> List.map (\( k, v ) -> statBox k (v |> wins) (v |> List.length)))
                    ++ [ civFilterBox "Our Team (All of)" filters.heroCivs
                       , civFilterBox "Enemy Team (Any of)" filters.enemyCivs
                       ]
                )
