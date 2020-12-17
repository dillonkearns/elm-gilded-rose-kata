module GildedRoseTests exposing (all)

import Expect exposing (Expectation)
import GildedRose exposing (Item(..), updateQuality)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "GildedRose.updateQuality"
        [ test "normal item" <|
            \_ ->
                Expect.equal [ Item "NORMAL ITEM" 4 9 ] <|
                    updateQuality [ Item "NORMAL ITEM" 4 9 ]
        ]
