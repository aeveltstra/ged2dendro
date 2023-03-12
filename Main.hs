{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import qualified System.Environment as Env 
import qualified Data.Text
import qualified Parser (parse, toTree, filterTree)
import qualified Writer (toXml)
import qualified Testing (runTests)

defaultAndThis :: Data.Text.Text
defaultAndThis = ""

defaultMaxDepth :: Integer
defaultMaxDepth = 2

{- | 
     Turns a GED text tree document into a dendrograph XML for DrawIO.

     Export the GED text tree document from a service like Ancestry.com. A family tree like that is a manually created genealogy, that shows how people are related to each other, and when events happened like births, marriages, divorces, and deaths. It usually contains quite a bit of sensitive information and specific source references. 

     Anonymizing that and turning it into a dendrograph is an arduous process even for experienced artists. That's where this process aims to help. It attempts to anonymize the graph and set up an initial diagram that can be imported into DrawIO for further editing.
     
     Provide 3 optional arguments: 
     1. (Optional) The @P or @F identifier to use as the end of the dendrograph.If not provided, the program will output all families and children who have children.
     2. (Optional) How many siblings and cousins wide the graph can go for each parent node. If not provided, the process will output all. That may be too much for larger trees!
     3. (Optional) How many ancestors up it can go. If not provided, the process will output 2 generations of ancestors (parents and grandparents).

     Pipe in the GED text tree document as stdin.

     The process validates the input file and arguments to make sure they contain expected input. Side effects: validation errors get pushed to stderr. It writes the dendrograph XML result to stdout, where you can catch it and forward it into a file.

-}
main :: IO ()
main = do
    args <- Env.getArgs --to pick up the user's arguments
    contents <- getContents --to pick up the GED document piped in from stdin 
    let l = case (null contents) of 
              True -> error ("Pipe the GED tree text doc "
                           ++ "into the stdin of this program. "
                           ++ "Aborting: no input document "
                           ++ "received.")
              False -> 0::Integer
        a = case (length args) of 
              0  -> do
                     let endId = ""
                         andThis = defaultAndThis
                         maxDepth = defaultMaxDepth
                     (endId, andThis, maxDepth)
              1 -> do
                     let endId = parseEndId (args!!0)
                         andThis = defaultAndThis
                         maxDepth = defaultMaxDepth
                     (endId, andThis, maxDepth)
              2 -> do
                     let endId = parseEndId (args!!0)
                         andThis = parseAndThis (args!!1)
                         maxDepth = defaultMaxDepth
                     (endId, andThis, maxDepth)
              _  -> do
                     let endId = parseEndId (args!!0)
                         andThis = parseAndThis (args!!1)
                         maxDepth = parseMaxDepth (args!!2)
                     (endId, andThis, maxDepth)
        lined = lines contents
        packed = fmap Data.Text.pack lined
        parsed = Parser.parse packed
        tree = Parser.toTree parsed 
        filtered = Parser.filterTree tree a
        xml = Writer.toXml filtered a
        output = Data.Text.unlines xml
    putStr $ Data.Text.unpack output

parseEndId :: String -> Data.Text.Text
parseEndId "" = ""
parseEndId x = Data.Text.pack x

parseAndThis :: String -> Data.Text.Text 
parseAndThis "" = ""
parseAndThis x = Data.Text.pack x

parseMaxDepth :: String -> Integer
parseMaxDepth "" = defaultMaxDepth
parseMaxDepth x = read x

test :: IO ()
test = do Testing.runTests

