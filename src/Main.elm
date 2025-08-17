module Main exposing (..)

import Browser
import Civilizations exposing (Civilization(..), civilizationToString)
import Dict
import GamesDecoder exposing (..)
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (attribute, checked, property, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode


type Season
    = Season Int


seasonFromString : String -> Season
seasonFromString s =
    case String.toInt s of
        Just v ->
            Season v

        Nothing ->
            Season 11


seasonStart : Season -> String
seasonStart season =
    case season of
        Season 11 ->
            "2025-06-26"

        Season 10 ->
            "2025-04-03"

        Season 9 ->
            "2024-10-01"

        Season 8 ->
            "2024-07-11"

        _ ->
            "2024-03-12"


type alias GameDict =
    Dict.Dict String (List Game)


type Msg
    = GotGames Season (Result Http.Error Games)
    | SetSeason Season
    | Toggle Side Civilization


wins : List Game -> Int
wins games =
    games |> List.map (\game -> game |> .result) |> List.filter (\result -> result == Win) |> List.length


losses : List Game -> Int
losses games =
    games |> List.map (\game -> game |> .result) |> List.filter (\result -> result == Loss) |> List.length


getGames : Int -> Season -> Cmd Msg
getGames page season =
    Http.get
        { url = "https://aoe4world.com/api/v0/players/76561197985789866/games?leaderboard=rm_3v3&since=" ++ seasonStart season ++ "&page=" ++ String.fromInt page
        , expect = Http.expectJson (GotGames season) gamesDecoder
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
    = Failure Http.Error
    | Loading Season
    | Success ( List Game, AllFilters, Season )


type Side
    = Hero
    | Enemy


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading (Season 11)
    , getGames 1 (Season 11)
    )



-- UPDATE


gameFilter : Season -> Game -> Bool
gameFilter (Season season) game =
    game.result
        /= Unknown
        && fullTeam game
        && game.season
        == season


validGames : Season -> List Game -> List Game
validGames season games =
    games |> List.filter (gameFilter season)


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


needMore : Games -> Bool
needMore response =
    response.offset + response.count < response.total


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( old_games, filters, season ) =
            case model of
                Success ( last_games, last_filters, season_ ) ->
                    ( last_games, last_filters, season_ )

                _ ->
                    ( []
                    , AllFilters (CivilizationFilter Hero List.all []) (CivilizationFilter Enemy List.any [])
                    , Season 11
                    )
    in
    case msg of
        GotGames season_ result ->
            case result of
                Ok response ->
                    ( Success ( old_games ++ (response.games |> validGames season), filters, season_ )
                    , if needMore response then
                        getGames (response.page + 1) season_

                      else
                        Cmd.none
                    )

                Err error ->
                    ( Failure error, Cmd.none )

        Toggle side civ ->
            case model of
                Success ( games, filters_, season_ ) ->
                    ( Success
                        ( games
                        , case side of
                            Hero ->
                                { filters_ | heroCivs = toggleCiv civ filters_.heroCivs }

                            Enemy ->
                                { filters_ | enemyCivs = toggleCiv civ filters_.enemyCivs }
                        , season_
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetSeason season_ ->
            ( Loading season_
            , getGames 1 season_
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


bgDark : Html.Attribute msg
bgDark =
    style "background-color" "#1B1B1E"


bgDarkBlue : Html.Attribute msg
bgDarkBlue =
    style "background-color" "#0D47A1"


bgLight : Html.Attribute msg
bgLight =
    style "background-color" "#232327"


textColor : Html.Attribute msg
textColor =
    style "color" "#E9E7E1"


header : String -> List (Html.Attribute Msg) -> Html Msg
header t styles =
    div
        ([ style "font-family" "'Brush Script MT', cursive"
         , style "font-size" "2em"
         , style "font-weight" "bold"
         ]
            ++ styles
        )
        [ text t ]


summaryText : Int -> Int -> Float -> String
summaryText numWins totalGames winRate =
    let
        percentage =
            (winRate |> String.fromFloat |> String.left 6) ++ "%"

        count =
            (numWins |> String.fromInt) ++ " wins out of " ++ (totalGames |> String.fromInt) ++ " games."
    in
    percentage ++ " (" ++ count ++ ")"


statBox : String -> Int -> Int -> Html Msg
statBox name numWins totalGames =
    let
        winRate =
            if totalGames == 0 then
                0

            else
                100 * (numWins |> toFloat) / (totalGames |> toFloat)
    in
    div [ bgLight, style "margin" "1em", style "padding" "1em", style "height" "2.5em" ]
        [ div
            []
            [ header (name ++ ":") [ style "display" "inline-block" ]
            , div
                [ style "font-size" "1.5em", style "float" "right" ]
                [ text (summaryText numWins totalGames winRate) ]
            ]
        , div [ bgDarkBlue ]
            [ div
                [ bgLight, style "width" (String.append ((100 - winRate) |> String.fromFloat) "%"), style "height" "1em" ]
                []
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
        box =
            \c -> civbox c filters.side (List.member c filters.civs)
    in
    div
        [ style "border-radius" "0.5em"
        , style "padding" "0.2em"
        , style "margin" "0.2em"
        , bgLight
        , style "width" "16em"
        ]
        [ header title []
        , box AbbasidDynasty
        , box Ayyubids
        , box Byzantines
        , box Chinese
        , box DelhiSultanate
        , box English
        , box French
        , box HolyRomanEmpire
        , box HouseOfLancaster
        , box Japanese
        , box JeanneDarc
        , box KnightsTemplar
        , box Malians
        , box Mongols
        , box OrderOfTheDragon
        , box Ottomans
        , box Rus
        , box ZhuXisLegacy
        ]


seasonDropdown : List (Html Msg)
seasonDropdown =
    List.range 8 11 |> List.map (\i -> option [ attribute "value" (String.fromInt i) ] [ text ("Season " ++ String.fromInt i) ]) |> List.reverse


view : Model -> Html Msg
view model =
    case model of
        Failure error ->
            let
                errorstr =
                    case error of
                        Http.BadUrl url ->
                            "Bad url: " ++ url

                        Http.Timeout ->
                            "Request timeout"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadStatus status ->
                            "Got status: " ++ String.fromInt status

                        Http.BadBody body ->
                            "Got bad html body: " ++ body
            in
            "Unable to load games from aoe4world: " ++ errorstr |> text

        Loading (Season season) ->
            text ("Loading Season " ++ String.fromInt season ++ "...")

        Success ( games, filters, Season season ) ->
            let
                gamesPerMap =
                    mapMaps games filters

                totalGames =
                    List.filter (applyFilters filters) games
            in
            div
                [ bgDark
                , style "min-height" "100vh"
                , style "font-family" "'Times New Roman', serif"
                , style "font-size" "1.1em"
                , textColor
                ]
                [ div
                    [ style "display" "inline-block"
                    , style "width" "75%"
                    , bgLight
                    , style "margin-left" "3em"
                    , style "height" "100vh"
                    ]
                    (statBox
                        "Total"
                        (totalGames |> wins)
                        (totalGames |> List.length)
                        :: (gamesPerMap |> Dict.toList |> List.map (\( k, v ) -> statBox k (v |> wins) (v |> List.length)))
                    )
                , div
                    [ style "display" "inline-block"
                    , style "position" "absolute"
                    , style "right" "1em"
                    , style "top" "1em"
                    ]
                    [ div
                        [ style "padding-bottom" "1em" ]
                        [ select
                            [ style "width" "100%"
                            , bgLight
                            , style "color-scheme" "dark"
                            , onInput (seasonFromString >> SetSeason)
                            , property "value" (String.fromInt season |> Json.Encode.string)
                            ]
                            seasonDropdown
                        ]
                    , civFilterBox "Our Team (All of)" filters.heroCivs
                    , civFilterBox "Enemy Team (Any of)" filters.enemyCivs
                    ]
                ]
