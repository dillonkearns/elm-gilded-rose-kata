module GildedRose exposing (Item(..), updateQuality)


type alias GildedRose =
    List Item


type Item
    = Item String Int Int


updateQuality : GildedRose -> GildedRose
updateQuality =
    List.map updateQualityItem


updateQualityItem : Item -> Item
updateQualityItem ((Item name _ _) as item) =
    case name of
        "Sulfuras, Hand of Ragnaros" ->
            updateSulfuras item

        "Backstage passes to a TAFKAL80ETC concert" ->
            item
                |> decreaseSellIn
                |> updateBackstagePasses
                |> floorQuality

        "Aged Brie" ->
            item
                |> decreaseSellIn
                |> updateBrie

        _ ->
            item
                |> decreaseSellIn
                |> updateRegularItem


updateRegularItem : Item -> Item
updateRegularItem (Item name sellIn quality) =
    let
        quality_ =
            if quality <= 0 then
                quality

            else
                quality - 1
    in
    if sellIn < 0 && quality_ > 0 then
        Item name sellIn (quality_ - 1)

    else
        Item name sellIn quality_


decreaseSellIn : Item -> Item
decreaseSellIn (Item name sellIn quality) =
    Item name (sellIn - 1) quality


increaseQuality : Bool -> Item -> Item
increaseQuality check ((Item name sellIn quality) as item) =
    if check then
        Item name sellIn (quality + 1)

    else
        item


updateSulfuras : Item -> Item
updateSulfuras ((Item name sellIn quality) as item) =
    let
        check =
            sellIn >= 0 && quality < 50
    in
    increaseQuality check item


checkSellInWithLowQuality : Item -> Bool
checkSellInWithLowQuality (Item _ sellIn quality) =
    sellIn >= 0 && quality < 50


updateBackstagePasses : Item -> Item
updateBackstagePasses (Item name sellIn quality) =
    let
        quality_ =
            if quality >= 50 then
                quality

            else if sellIn >= 10 then
                quality + 1

            else if sellIn < 5 && quality < 48 then
                quality + 3

            else if quality < 49 then
                quality + 2

            else
                quality + 1
    in
    Item name sellIn quality_


resetQuality : Item -> Item
resetQuality (Item name sellIn _) =
    Item name sellIn 0


floorQuality : Item -> Item
floorQuality ((Item _ sellIn _) as item) =
    if sellIn < 0 then
        resetQuality item

    else
        item


updateBrie : Item -> Item
updateBrie (Item name sellIn quality) =
    let
        quality_ =
            if quality < 50 then
                quality + 1

            else
                quality
    in
    if sellIn < 0 && quality_ < 50 then
        Item name sellIn (quality_ + 1)

    else
        Item name sellIn quality_
