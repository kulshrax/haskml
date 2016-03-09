{-# LANGUAGE OverloadedStrings, TupleSections #-}

module ParseHtml (parseHtml, textToHtml) where

import qualified Data.Text as T
import qualified Data.Map as M

import Data.String

import Text.Parsec
import Text.Parsec.Text

import Html


instance IsString Html where
    fromString = textToHtml . T.pack


textToHtml :: T.Text -> Html
textToHtml input = 
    case parseHtml input of
        Left e -> error . show $ e
        Right h -> h


parseHtml :: T.Text -> Either ParseError Html
parseHtml = parse html "HaskML" 


html :: Parser Html
html = Html <$> many (try element <|> try comment <|> try text)


element :: Parser Node
element = do
    elem <- startTag
    content <- html
    end <- endTag
    if end /= _tagName elem
        then fail "Tag name mismatch."
        else return $ elem { _innerHtml = content }


text :: Parser Node
text = do
    first <- anyChar
    rest <- manyTill anyChar $ lookAhead (char '<')
    return . Text . T.pack $ first:rest


startTag :: Parser Node
startTag = do
    char '<'
    tag <- tagName
    spaces
    attributes <- attrList
    spaces
    char '>'
    return $ Element tag attributes mempty

endTag :: Parser T.Text
endTag = string "</" *> tagName <* char '>'

tagName :: Parser T.Text
tagName = T.toLower . T.pack <$> many1 alphaNum


attrList :: Parser Attributes
attrList = M.fromList <$> sepBy attribute (many1 space >> lookAhead attribute)

attribute :: Parser (T.Text, T.Text)
attribute = try attrPair <|> attrEmpty

attrEmpty :: Parser (T.Text, T.Text)
attrEmpty = (,"") <$> attrName

attrPair :: Parser (T.Text, T.Text)
attrPair = (,) <$> attrName <* char '=' <*> attrValue

attrName :: Parser T.Text
attrName = T.toLower . T.pack <$> (many1 $ alphaNum <|> char '-')

attrValue :: Parser T.Text
attrValue = try attrSingle <|> try attrDouble <|> attrUnquoted

attrSingle :: Parser T.Text
attrSingle = T.pack <$> (char '\'' *> manyTill anyChar (char '\''))

attrDouble :: Parser T.Text
attrDouble = T.pack <$> (char '"' *> manyTill anyChar (char '"'))

attrUnquoted :: Parser T.Text
attrUnquoted = T.pack <$> (manyTill anyChar $ lookAhead (space <|> char '>'))


comment :: Parser Node
comment = Comment <$> (string "<!--" *> commentBody <* string "-->")

commentBody :: Parser T.Text
commentBody = T.pack <$> (manyTill anyChar $ lookAhead (string "-->"))
