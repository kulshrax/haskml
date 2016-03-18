{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Html
import RenderHtml
import ParseHtml
import Combinator
import qualified Tags as H
import qualified Attributes as A

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Test.Hspec


main :: IO ()
main = hspec $ do
    parserSpec
    rendererSpec
    htmlSpec
    combinatorSpec


parserSpec :: Spec
parserSpec = describe "ParseHtml" $ do
    it "Parses text nodes" $
        textToHtml "test" `shouldBe` Html [Text "test"]

    it "Parses simple elements" $
        textToHtml "<a></a>" `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "a",
                    attributes = M.empty, 
                    content = InnerHtml (
                        Html {getNodes = []}
                    )
                }
            ]
        }

    it "parses elements with contents" $
        textToHtml "<a>Test</a>" `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "a",
                    attributes = M.empty, 
                    content = InnerHtml (
                        Html {getNodes = [Text "Test"]}
                    )
                }
            ]
        }

    it "parses elements with attributes" $
        textToHtml "<a id=\"link\" href=\"google.com\"></a>" `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "a",
                    attributes = M.fromList [
                        ("id", "link"),
                        ("href", "google.com")
                    ],  
                    content = InnerHtml (
                        Html {getNodes = []}
                    )
                }
            ]
        }

    it "parses sibling elements" $
        textToHtml "<a><span></span></a>" `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "a",
                    attributes = M.empty,  
                    content = InnerHtml (
                        Html {getNodes = [
                            Element {
                                tag = "span",
                                attributes = M.fromList [],  
                                content = InnerHtml (
                                    Html {getNodes = []})}
                        ]}
                    )
                }
            ]
        }

    it "parses nested elements" $
        textToHtml "<a></a><span></span>" `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "a",
                    attributes = M.empty,  
                    content = InnerHtml (
                        Html {getNodes = []}
                    )
                },
                Element {
                    tag = "span",
                    attributes = M.fromList [],  
                    content = InnerHtml (
                        Html {getNodes = []}
                    )
                }
            ]
        }

    it "parses comments" $
        textToHtml "<!-- Comment -->" `shouldBe` Html [Comment " Comment "]


rendererSpec :: Spec
rendererSpec = describe "RenderHtml" $ do
    it "Renders text nodes" $
        render (Html [Text "test"]) `shouldBe` "test"

    let inverse = \x -> (render . textToHtml) x `shouldBe` x 

    it "Inverts parsing text nodes" $ 
        inverse "test"

    it "Inverts parsing comments" $ 
        inverse "<!-- Comment -->"

    it "Inverts parsing elements" $ 
        inverse "<a>test</a>"

    it "Inverts parsing nested siblings" $ 
        inverse "<div><div>foo</div><div>bar</div></div>"

    it "Inverts parsing attributes" $ 
        inverse "<a href=\"google.com\">link</a>"


htmlSpec :: Spec
htmlSpec = describe "Html" $ do
    it "Works with OverloadedStrings" $
        ("<p>test</p>" :: Html) `shouldBe` Html {
            getNodes = [
                Element {
                    tag = "p",
                    attributes = M.empty, 
                    content = InnerHtml (
                        Html {getNodes = [Text "test"]}
                    )
                }
            ]
        }

    it "Supports concatenation." $
        Html [Text "foo"] <> Html [Text "bar"] `shouldBe` 
            Html [Text "foo", Text "bar"]


combinatorSpec :: Spec
combinatorSpec = describe "Combinator" $ do
    it "Single combinator" $
        show H.p `shouldBe` "<p></p>"

    it "Sibling combinator" $
        let siblings = do
                        H.p "foo"
                        H.div "bar"
        in show siblings `shouldBe` "<p>foo</p><div>bar</div>"

    it "Nested combinator" $
        show (H.p . H.div) `shouldBe` "<p><div></div></p>"

    it "Supports attributes" $
        show (H.img ! A.src "image.png" ! A.alt "Image") `shouldBe`
            "<img src=\"image.png\" alt=\"Image\">"

    it "Works with OverloadedStrings" $
        show (H.p $ "<a>Google</a>" ! A.href "google.com") `shouldBe` 
            "<p><a href=\"google.com\">Google</a></p>"
