module Civilizations exposing (Civilization(..), civilizationFromString, civilizationToString)


type Civilization
    = AbbasidDynasty
    | Ayyubids
    | Byzantines
    | Chinese
    | DelhiSultanate
    | English
    | French
    | HolyRomanEmpire
    | HouseOfLancaster
    | Japanese
    | JeanneDarc
    | KnightsTemplar
    | Malians
    | Mongols
    | OrderOfTheDragon
    | Ottomans
    | Rus
    | ZhuXisLegacy


civilizationFromString : String -> Maybe Civilization
civilizationFromString civ =
    case civ of
        "abbasid_dynasty" ->
            Just AbbasidDynasty

        "ayyubids" ->
            Just Ayyubids

        "byzantines" ->
            Just Byzantines

        "chinese" ->
            Just Chinese

        "delhi_sultanate" ->
            Just DelhiSultanate

        "english" ->
            Just English

        "french" ->
            Just French

        "holy_roman_empire" ->
            Just HolyRomanEmpire

        "house_of_lancaster" ->
            Just HouseOfLancaster

        "japanese" ->
            Just Japanese

        "jeanne_darc" ->
            Just JeanneDarc

        "knights_templar" ->
            Just KnightsTemplar

        "malians" ->
            Just Malians

        "mongols" ->
            Just Mongols

        "order_of_the_dragon" ->
            Just OrderOfTheDragon

        "ottomans" ->
            Just Ottomans

        "rus" ->
            Just Rus

        "zhu_xis_legacy" ->
            Just ZhuXisLegacy

        _ ->
            Nothing


civilizationToString : Civilization -> String
civilizationToString civ =
    case civ of
        AbbasidDynasty ->
            "Abbasi Dynasty"

        Ayyubids ->
            "Ayyubids"

        Byzantines ->
            "Byzantines"

        Chinese ->
            "Chinese"

        DelhiSultanate ->
            "Delhi Sultanate"

        English ->
            "English"

        French ->
            "French"

        HolyRomanEmpire ->
            "Holy Roman Empire"

        HouseOfLancaster ->
            "House of Lancaster"

        Japanese ->
            "Japanese"

        JeanneDarc ->
            "Jeanne Darc"

        KnightsTemplar ->
            "Knights Templar"

        Malians ->
            "Malians"

        Mongols ->
            "Mongols"

        OrderOfTheDragon ->
            "Order of the Dragon"

        Ottomans ->
            "Ottomans"

        Rus ->
            "Rus"

        ZhuXisLegacy ->
            "Zhu Xi's Legacy"
