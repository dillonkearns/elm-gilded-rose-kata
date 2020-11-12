module GildedRose exposing (Item(..), updateQuality)


type alias GildedRose =
    List Item


type Item
    = Item String Int Int


updateQuality : GildedRose -> GildedRose
updateQuality =
    List.map updateQualityItem


updateQualityItem : Item -> Item
updateQualityItem (Item name sellIn quality) =
    let
        quality_ =
            if name /= "Aged Brie" then
                if quality <= 0 then
                    quality

                else
                    quality - 1

            else if quality < 50 then
                quality + 1

            else
                quality

        sellIn_ =
            sellIn - 1

        result =
            if sellIn_ < 0 then
                if name /= "Aged Brie" then
                    if quality_ > 0 then
                        Item name sellIn_ (quality_ - 1)

                    else
                        Item name sellIn_ quality_

                else if quality_ < 50 then
                    Item name sellIn_ (quality_ + 1)

                else
                    Item name sellIn_ quality_

            else
                Item name sellIn_ quality_
    in
    if name == "Sulfuras, Hand of Ragnaros" then
        updateSulfuras (Item name sellIn quality)

    else if name == "Backstage passes to a TAFKAL80ETC concert" then
        updateBackstagePasses (Item name sellIn quality)

    else if name == "Aged Brie" then
        updateBrie (Item name sellIn quality)

    else
        result


updateSulfuras : Item -> Item
updateSulfuras (Item name sellIn quality) =
    if sellIn < 0 || quality >= 50 then
        Item name sellIn quality

    else
        Item name sellIn (quality + 1)


updateBackstagePasses : Item -> Item
updateBackstagePasses (Item name sellIn quality) =
    let
        quality_ =
            if quality >= 50 then
                quality

            else if sellIn >= 11 then
                quality + 1

            else if sellIn < 6 && quality < 48 then
                quality + 3

            else if quality < 49 then
                quality + 2

            else
                quality + 1

        sellIn_ =
            sellIn - 1
    in
    if sellIn_ < 0 then
        Item name sellIn_ 0

    else
        Item name sellIn_ quality_


updateBrie : Item -> Item
updateBrie (Item name sellIn quality) =
    let
        quality_ =
            if quality < 50 then
                quality + 1

            else
                quality

        sellIn_ =
            sellIn - 1

        result =
            if sellIn_ < 0 then
                if name /= "Aged Brie" then
                    if quality_ > 0 then
                        Item name sellIn_ (quality_ - 1)

                    else
                        Item name sellIn_ quality_

                else if quality_ < 50 then
                    Item name sellIn_ (quality_ + 1)

                else
                    Item name sellIn_ quality_

            else
                Item name sellIn_ quality_
    in
    if name == "Sulfuras, Hand of Ragnaros" then
        updateSulfuras (Item name sellIn quality)

    else if name == "Backstage passes to a TAFKAL80ETC concert" then
        updateBackstagePasses (Item name sellIn quality)

    else
        result
