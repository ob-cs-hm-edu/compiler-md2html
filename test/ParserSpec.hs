module ParserSpec (spec) where

import           Test.Hspec

import           Data.Maybe (fromJust)
import           Parser
import           Types

spec :: Spec
spec =
    describe "The Parser" $ do

        it "parses the empty stream" $
            fromJust ( parse []) `shouldBe` Sequence []

        it "parses a newline" $
            fromJust ( parse [TokenNewline])
                `shouldBe` Sequence [P [Text "\n"]]

        it "parses 2 Newlines" $
            fromJust ( parse [TokenNewline, TokenNewline])
                `shouldBe` Sequence [Emptyline]

        it "parses 2 Texts" $
            fromJust ( parse [TokenText "Hallo", TokenNewline])
                `shouldBe` Sequence [P [Text "Hallo", Text "\n"]]

        it "does not parse a header without a following blank" $
            fromJust ( parse [TokenH 6, TokenText "Hallo"])
                `shouldBe` Sequence [P [Text "######", Text "Hallo"]]

        it "parses a header" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!", TokenNewline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    , P [Text "\n" ]
                                    ]

        it "parses a header without a newline" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!"])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    ]

        it "parses a header with two newlines" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!"
                              ,TokenNewline, TokenNewline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"]
                                          )
                                    , Emptyline
                                    ]

        it "parses two Paragraphs" $
            fromJust ( parse [ TokenText "Hallo", TokenNewline, TokenText "Welt"
                              , TokenBlanks 3, TokenNewline, TokenNewline
                              , TokenText "Huhu", TokenBlanks 1, TokenText "World"
                              , TokenNewline])
                `shouldBe` Sequence [ P [Text "Hallo", Text "\n"
                                        , Text "Welt", Text "   "
                                        ]
                                    , Emptyline
                                    , P [Text "Huhu", Text " "
                                        , Text "World", Text "\n"
                                        ]
                                    ]
