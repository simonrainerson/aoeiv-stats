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
    | Japanese
    | JeanneDarc
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

        "japanese" ->
            Just Japanese

        "jeanne_darc" ->
            Just JeanneDarc

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

        Japanese ->
            "Japanese"

        JeanneDarc ->
            "Jeanne Darc"

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
