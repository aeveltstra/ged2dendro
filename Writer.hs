{-# LANGUAGE OverloadedStrings #-}
module Writer (toXml) where

import Prelude
import qualified Data.List
import qualified Data.Text as DT
import qualified Parser as P

xml_declaration, xml_closer :: DT.Text
xml_declaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<xml>"
xml_closer = "</xml>"

{- | Turns branches into raw XML, so that from then on, we can use
     XSLT instead of linear parsing.
     Filters the input tree by the shape of the passed-in limitations.
     See Writer.filter.

     Input parameters:
     1.  The list of branches returned by P.toTree.
     2.1 EndId specifies which person or family got targeted 
         by the user, and needs to be formatted to stand out.
     2.2 AndThis specifies additional nodes that got targeted
         by the user, and need to be formatted differently.
     2.3 MaxDepth is not used.
-}
toXml :: [P.Branch] -> (P.EndId, P.AndThis, P.MaxDepth) -> [DT.Text]
toXml [] _  = [xml_declaration, xml_closer]
toXml [x] (e, a, _) = xml_declaration 
                      : ((branchToXml e a) x) 
                      : xml_closer : []
toXml xs (e, a, _)  = xml_declaration 
                      : xmltexts ++ [xml_closer] where 
                          xmltexts = fmap (branchToXml e a) xs

{- | Translates a property type into a tag name. -}
tag :: P.PropertyType -> DT.Text
tag (P.UnknownPropertyType) = "unknown-type"
tag (P.NodeIdentifierProperty _) = "id"
tag (P.ValuedProperty name) 
  | name == "CHAR" = "charset"
  | name == "CORP" = "corporation"
  | name == "PROB" = "probate"
  | name == "TITL" = "title"
  | name == "_FREL" = "relation-to-father"
  | name == "_MREL" = "relation-to-mother"
  | name == "PLAC" = "place"
  | name == "RESI" = "residence"
  | name == "SEX"  = "sexe"
  | name == "VERS" = "version"
  | otherwise = DT.toLower name
tag (P.PlaceCategoryProperty name) 
  | name == P.BirthPlace = "birth"
  | name == P.DeathPlace = "death"
  | name == P.BurialPlace = "burial"
  | otherwise = "unknown-place-type"
tag (P.EmptyProperty name) 
  | name == "HEAD" = "head"
  | name == "DATA" = "data"
  | name == "DIV"  = "division"
  | name == "MARR" = "marriage"
  | name == "_MILT" = "military"
  | name == "OBJE" = "object"
  | name == "PROB" = "prob"
  | name == "TRLR" = "trailer"
  | otherwise = DT.toLower name
tag (P.ReferenceProperty name) 
  | name == "CHIL" = "child"
  | name == "FAMC" = "parents"
  | name == "FAMS" = "own-family"
  | name == "HUSB" = "spouse"
  | name == "REPO" = "repository"
  | name == "SOUR" = "source"
  | name == "WIFE" = "spouse"
  | otherwise = "unknown-ref-type"

{- | Converts a raw & into the html entity &amp; -}
amp :: DT.Text -> DT.Text
amp v = DT.replace "&amp;amp;" "&amp;" (DT.replace "&" "&amp;" v)

{- | Converts a raw < into the html entity &lt; -}
lt :: DT.Text -> DT.Text
lt v = DT.replace "&amp;lt;" "&lt;" (DT.replace "<" "&lt;" v)

{- | Converts a raw > into the html entity &gt; -}
gt :: DT.Text -> DT.Text
gt v = DT.replace "&amp;gt;" "&gt;" (DT.replace ">" "&gt;" v)

{- | Removes the @ from values. -}
noAt :: DT.Text -> DT.Text
noAt v = DT.replace "@" "" v

{- | Removes the / from values. -}
noSlash :: DT.Text -> DT.Text
noSlash v = DT.replace "/" "" v

{- | Substitutes common characters -}
sub :: DT.Text -> DT.Text
sub "" = ""
sub v 
  | "@" == (DT.take 1 v) = noAt v
  | otherwise = amp $ lt $ gt v

{- | Finds the correct tag for the property type and
   wraps it in < and >. -}
startTag :: P.PropertyType -> DT.Text
startTag t = DT.intercalate (tag t) ["<", ">"]

{- | Finds the correct tag for the property type and
   wraps it in </ and >. -}
endTag :: P.PropertyType -> DT.Text
endTag t = DT.intercalate (tag t) ["</", ">"]

{- | Turns a single leaf into a single raw xml element -}
leafToXml :: P.Leaf -> DT.Text
leafToXml (P.Leaf t v []) = DT.intercalate (sub v) [
    startTag t,
    endTag t
  ]
leafToXml (P.Leaf t v children) = DT.intercalate "" [
    startTag t,
    sub v,
    (DT.intercalate "" (fmap leafToXml children)),
    endTag t
  ]

{- | Turns a single person leaf into a single raw xml element -}
personLeafToXml :: P.Leaf -> DT.Text
personLeafToXml (P.Leaf t v []) 
  | t == P.p_name = DT.intercalate (noSlash $ sub v) [
                           "<name>", 
                           "</name>"
                         ]
  | otherwise = DT.intercalate (sub v) [
                           startTag t,
                           endTag t
                         ]

personLeafToXml (P.Leaf t v children)
  | t == P.p_name = DT.intercalate "" [
                           "<name>",
                           noSlash $ sub v,
                           (DT.intercalate "" (fmap personLeafToXml children)),
                           "</name>" 
                         ]
  | otherwise = DT.intercalate "" [
                           startTag t,
                           sub v,
                           (DT.intercalate "" (fmap personLeafToXml children)),
                           endTag t
                         ]


{- | Figure out which formatting hints to add. -}
addFormatHints :: P.EndId -> P.AndThis -> P.Branch -> DT.Text
addFormatHints _ _ (P.Branch _ []) = ""
addFormatHints e a (P.Branch P.FamilyNode l) 
  = DT.concat [e',  a'] where
  e' = case e == P.getLeafId (P.findFamilyLeaf l) of 
         True -> "<endNode/>"
         False -> ""
  a' = case Data.List.any (P.matchAndThisLeaf a) l of
         True -> "<andThisNode/>"
         False -> ""
addFormatHints e a (P.Branch P.PersonNode l) 
  = DT.concat [e',  a'] where
  e' = case e == P.getLeafId (P.findPersonLeaf l) of 
         True -> "<endNode/>"
         False -> ""
  a' = case Data.List.any (P.matchAndThisLeaf a) l of
         True -> "<andThisNode/>"
         False -> ""
addFormatHints _ a (P.Branch _ l) = a' where  
  a' = case Data.List.any (P.matchAndThisLeaf a) l of
         True -> "<andThisNode/>"
         False -> ""

{- | Turns a single branch into a single raw xml element. -}
branchToXml :: P.EndId -> P.AndThis -> P.Branch -> DT.Text
branchToXml _ _ (P.Branch P.HeaderNode leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    "<header>",
    "</header>\n"
  ]
branchToXml _ _ (P.Branch P.TrailerNode leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    "<trailer>",
    "</trailer>\n"
  ]
branchToXml e a (P.Branch P.SourceNode leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    DT.concat ["<source>", f],
    "</source>\n"
  ] where 
     f = addFormatHints e a (P.Branch P.SourceNode leaves)
branchToXml e a (P.Branch P.FamilyNode leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    DT.concat ["<family>", f],
    "</family>\n"
  ] where 
     f = addFormatHints e a (P.Branch P.FamilyNode leaves)
branchToXml e a (P.Branch P.PersonNode leaves) = DT.intercalate (DT.concat (fmap personLeafToXml leaves)) [
    DT.concat ["<person>", f],
    "</person>\n"
  ] where
     f = addFormatHints e a (P.Branch P.PersonNode leaves)
branchToXml e a (P.Branch P.RepositoryNode leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    DT.concat ["<repository>", f],
    "</repository>\n"
  ] where 
     f = addFormatHints e a (P.Branch P.RepositoryNode leaves)
branchToXml e a (P.Branch P.UnknownNodeType leaves) = DT.intercalate (DT.concat (fmap leafToXml leaves)) [
    DT.concat ["<unknown-node>", f],
    "</unknown-node>\n"
  ] where 
     f = addFormatHints e a (P.Branch P.UnknownNodeType leaves)
