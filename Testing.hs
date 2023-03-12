{-# LANGUAGE OverloadedStrings #-}
module Testing where

import Prelude
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Parser (
                   Branch(..),
                   countBranches, 
                   filterTree,
                   findEndNode,
                   findParents,
                   GEDExpression(..), 
                   getBranchId,
                   hasUnknownProperty, 
                   headExpression, 
                   isBranch, 
                   isHeader, 
                   isNodePropertyTypeUnknown,
                   Leaf(..), 
                   matchAndThisLeaf,
                   NodeProperty(..), 
                   parse, 
                   parseLine, 
                   p_husb,
                   p_titl, 
                   p_wife, 
                   --PropertyType(..), 
                   --toBranches, 
                   toLeaf, 
                   toBranch, 
                   toLeaves,
                   --toNodeType, 
                   toTree
                 )
import qualified Writer (
                   toXml
                 )

data TestResult = 
  Failed 
  | Succeeded 
  deriving (Bounded, Eq, Read, Show)

toResult :: Bool -> TestResult
toResult True = Succeeded
toResult False = Failed

{- | Runs all unit tests (not the integration tests)
     and outputs whether they succeeded or failed,
     with the test name, and a totalization.
     Example invocation: Testing.runTests
     Example output: 
         Percentage successful: 100
         Percentage failed: 0
         Testing canParseHead: Succeeded
         Testing parsingHeadShowsHead: Succeeded
-}
runTests :: IO ()
runTests = putStrLn q where 
    t = [   canParseHead
          , parsingHeadShowsHead
          , canDetectParsedExpressionIsHeader
          , canDetectConstructedExpressionIsHeader
          , parsingCharShowsChar
          , parsingNameShowsName
          , parsingIndiNodeIdentityShowsNodeIdentity
          , parsingFamNodeIdentityShowsNodeIdentity
          , parsingSourNodeIdentityShowsNodeIdentity
          , canMakeLeaf
          , canShowLeaf
          , canDetectUnknownPropertyType 
          , canConvertNodePropertyToLeaf 
          , canConvertNodePropertiesToBranchLeaves 
          , canTurnBranchExpressionIntoBranch 
          , cannotTurnLeafExprIntoBranch 
          , canMorphValidSmallDocIntoNodes
          , canCreateRawXml 
          , canMatchAndThisLeaf
          , canGetBranchId
          , canFindEndIdInTree 
          , canFilterToFindNone 
          , canFilterToFindPerson 
          , canFilterToFindGrandparents 
          , canFindParentFamily 
          , canFindParentsOfFamily 
        ]
    n = [   "canParseHead"
          , "parsingHeadShowsHead"
          , "canDetectParsedExpressionIsHeader"
          , "canDetectConstructedExpressionIsHeader"
          , "parsingCharShowsChar"
          , "parsingNameShowsName"
          , "parsingIndiNodeIdentityShowsNodeIdentity"
          , "parsingFamNodeIdentityShowsNodeIdentity"
          , "parsingSourNodeIdentityShowsNodeIdentity"
          , "canMakeLeaf"
          , "canShowLeaf"
          , "canDetectUnknownPropertyType"
          , "canConvertNodePropertyToLeaf"
          , "canConvertNodePropertiesToBranchLeaves"
          , "canTurnBranchExpressionIntoBranch"
          , "cannotTurnLeafExprIntoBranch"
          , "canMorphValidSmallDocIntoNodes"
          , "canCreateRawXml" 
          , "canMatchAndThisLeaf"
          , "canGetBranchId"
          , "canFindEndIdInTree"
          , "canFilterToFindNone"
          , "canFilterToFindPerson"
          , "canFilterToFindGrandparents" 
          , "canFindParentFamily"
          , "canFindParentsOfFamily"
        ]
    m = length n
    r = fmap toResult t
    s = filter (Succeeded==) r
    q = Data.List.intercalate "\n" (sc:sf:o)
    ps = length s
    o = fmap out [0..m-1] where
      out i = concat ["Testing ", n!!i, ": ", show (r!!i)]
    cc = (round (
                  (100::Double) 
                  * (
                      (realToFrac ps) 
                      / (realToFrac m)
                    )::Double
                )
          )::Integer
    fc = (100 - cc)::Integer
    sc = "Percentage successful: " ++ (show cc)
    sf = "Percentage failed: " ++ (show fc)
    
{- | A helper function to reduce boilerplate coding.
     Example invocation: 
         let result = Testing.parseGEDLine (
                          Data.Text.pack "0 HEAD"
                      )
     Example output: Just GEDExpression
     Example output: Nothing
-}
parseGEDLine :: Data.Text.Text -> Maybe Parser.GEDExpression
parseGEDLine x = Parser.parseLine x


{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     The first line should be the HEAD property. If 
     the function Parser.parseLine encounters one, 
     it should recognize that and store no property value. 
     This function tests whether that happens at all.
     Example invocation: Testing.canParseHead
     Example outcome: True
-}
canParseHead :: Bool
canParseHead = Data.Maybe.isJust x where
  x = parseGEDLine (Data.Text.pack input)
  input = "0 HEAD"

{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     The first line should be the HEAD property. If 
     the function Parser.parseLine encounters one, 
     it should recognize that and store no property value. 
     This function tests not only whether such a line
     can be parsed, but also that it is recognized as 
     empty.
     Example invocation: Testing.parsingHeadShowsHead
     Example outcome: True
-}
parsingHeadShowsHead :: Bool
parsingHeadShowsHead = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "0 HEAD"
  expected = "0 EmptyProperty \"HEAD\" \"\""

{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     The first line should be the HEAD property. If 
     the function Parser.parseLine encounters one, 
     it should create a special GEDExpression that 
     contains a property (EmptyProperty "HEAD").
     This function tests whether Parser.isHeader can
     recognize that. 
     Example invocation: 
         Testing.canDetectParsedExpressionIsHeader
     Example outcome: True
-}
canDetectParsedExpressionIsHeader :: Bool
canDetectParsedExpressionIsHeader = z where
  input = "0 HEAD"
  x = parseGEDLine (Data.Text.pack input)
  y = Data.Maybe.fromJust x
  z = Parser.isHeader y

{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     The first line should be the HEAD property. If 
     the function Parser.parseLine encounters one, 
     it should create a special GEDExpression that 
     contains a property (EmptyProperty "HEAD").
     The function Parser.isHeader is designed to detect
     that. It does so by deconstructing the GEDExpression.
     This test also tests Parser.isHeader, but instead 
     of using a text line as input, it uses the construct.

     Example invocation: 
         Testing.canDetectConstructedExpressionIsHeader
     Example outcome: True
-}
canDetectConstructedExpressionIsHeader :: Bool
canDetectConstructedExpressionIsHeader = z where
  input = Parser.headExpression 
  z = Parser.isHeader input

{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     One such property is CHAR. If the function Parser.
     parseGEDLine encounters one, it should recognize 
     that and store the given property value. The value 
     can contain text, numbers, and symbols.
     Example invocation: Testing.parsingCharShowsChar
     Example outcome: True
-}
parsingCharShowsChar :: Bool
parsingCharShowsChar = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "1 CHAR UTF-8"
  expected = "1 ValuedProperty \"CHAR\" \"UTF-8\""

{- | A document has lines that starts with a number. 
     Each line that starts with a number higher than 0
     contains a property of a branch or node.
     One such property is NAME. If the function Parser.
     parseGEDLine encounters one, it should recognize 
     that and store the given property value. The value
     can contain text, numbers, symbols, and spaces.
     Example invocation: Testing.parsingNameShowsName
     Example outcome: True
-}
parsingNameShowsName :: Bool
parsingNameShowsName = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "1 NAME Home /Base/"
  expected = "1 ValuedProperty \"NAME\" \"Home /Base/\""

{- | A document has lines that start with the number 0.
     Those start a new branch, and contain identifiers.
     The function Parser.parseGEDLine must be able to 
     correctly identify a valid individual branch start line.
     Example invocation: Testing.parsingIndiNodeIdentityShowsNodeIdentity
     Example output: True
-}
parsingIndiNodeIdentityShowsNodeIdentity :: Bool
parsingIndiNodeIdentityShowsNodeIdentity = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "0 @P51@ INDI"
  expected = "0 NodeIdentifierProperty INDI \"@P51@\""

{- | A document has lines that start with the number 0.
     Those start a new branch, and contain identifiers.
     The function Parser.parseGEDLine must be able to 
     correctly identify a valid family branch start line.
     Example invocation: Testing.parsingFamNodeIdentityShowsNodeIdentity
     Example output: True
-}
parsingFamNodeIdentityShowsNodeIdentity :: Bool
parsingFamNodeIdentityShowsNodeIdentity = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "0 @F123@ FAM"
  expected = "0 NodeIdentifierProperty FAM \"@F123@\""

{- | A document has lines that start with the number 0.
     Those start a new branch, and contain identifiers.
     The function Parser.parseGEDLine must be able to 
     correctly identify a valid source line.
     Example invocation: Testing.parsingSourNodeIdentityShowsNodeIdentity
     Example output: True
-}
parsingSourNodeIdentityShowsNodeIdentity :: Bool
parsingSourNodeIdentityShowsNodeIdentity = (show z) == expected where
  z = Data.Maybe.fromJust x
  x = parseGEDLine (Data.Text.pack input)
  input = "0 @S876545678@ SOUR"
  expected = "0 NodeIdentifierProperty SOUR \"@S876545678@\""

{- | Helper function to reduce duplicate boilerplate code.
     Parses a GED text tree document specified as a file path,
     and returns the list of parsed expressions.
     Example invocation: 
         parsed <- Testing.parseFile "~/GED/48/tree.ged"
     Example outcome: [Parser.GEDExpression]
-}
parseFile :: String -> IO [Parser.GEDExpression]
parseFile location = do
    contents <- (readFile location)
    let asLines = lines contents
        packed = fmap Data.Text.pack asLines
        parsed = Parser.parse packed
    return parsed

{- | A document has lines with content in a specific format.
     It can also have lines with no content.
     The Parser.parse function must omit any lines that are
     empty or blank, and must be able to parse any valid line.
     That function returns only valid lines.
     Example invocation: 
     Testing.canParse "~/GED/50/50.ged" 20984
     Example output: Succeeded 
-}
canParseFile :: String -> Int -> IO TestResult
canParseFile location n = do 
    parsed <- (parseFile location) 
    let count = length parsed
        equalsExpectation = count == n
        outcome = if equalsExpectation then Succeeded else Failed
    return outcome 

{- | Run this on a valid file to ensure that the parser can
     parse all input lines. It should output all valid lines in 
     internal representation without throwing any errors.
-}   
showParsedFile :: String -> IO ()
showParsedFile location = do
    parsed <- (parseFile location)
    let shown = fmap show parsed
        out = Data.List.intercalate "\n" shown
    putStrLn out

{- | Validates the parser to ensure all lines of a valid file
     can be parsed properly, and none of the resulting 
     expressions contain an unknown property.
     This tests whether property detection is complete and 
     exhaustive during runtime.
-}
inputFileLacksUnknownProperty :: String -> IO TestResult
inputFileLacksUnknownProperty location = do
    parsed <- (parseFile location)
    let q = Parser.hasUnknownProperty parsed
        equalsExpectation = False == q
        outcome = if equalsExpectation then Succeeded else Failed
    return outcome

 
{- | A document has lines that start with a sequence number.
     Every line that starts with 0 starts a branch.
     Every subsequent line until the next 0 is a property 
     of the branch.
     This test tells whether the parser correctly counts the
     amount of branches in the document.
     Example invocation: 
     Testing.canCountBranches "/mnt/c/Users/aveltstra/GED/51/51.ged" 1784
     Example output: Succeeded 
-}
canCountBranches :: String -> Int -> IO TestResult
canCountBranches location n = do
    parsed <- (parseFile location) 
    let count = Parser.countBranches parsed 
        equalsExpectation = count == n
        outcome = if equalsExpectation then Succeeded else Failed
    return outcome 

{- | Produces a small valid GED document 
     that contains 9 branches, for testing purposes.
-}
produceValidDoc9Branches :: [Data.Text.Text]
produceValidDoc9Branches = [
    "0 HEAD"
  , "1 CHAR UTF-8"
  , "1 SOUR OmegaJunior Software Consultancy"
  , "1 GEDC"
  , "2 VERS 5.5"
  , "2 FORM LINEAGE-LINKED"
  , "0 @P5@ INDI"
  , "1 BIRT"
  , "2 DATE 1 Jan 1900"
  , "1 NAME Percy /Jackson/"
  , "1 SEX M"
  , "1 FAMC @F2@"
  , "0 @P1@ INDI"
  , "1 SEX F"
  , "1 NAME /GAIA/"
  , "1 OBJE"
  , "2 FILE https://images.cloudflare.com/gaia.jpg"
  , "2 FORM jpg"
  , "2 TITL Gaia"
  , "1 FAMS @F1@"
  , "0 @P2@ INDI"
  , "1 SEX M"
  , "1 NAME /Kronos/"
  , "1 FAMS @F1@"
  , "0 @P3@ INDI"
  , "1 BIRT"
  , "2 DATE 1 Jan 1900"
  , "1 SEX M"
  , "1 NAME /Poseidon/"
  , "1 FAMC @F1@"
  , "1 FAMS @F2@"
  , "0 @P4@ INDI"
  , "1 BIRT"
  , "2 DATE 2 Feb 1901"
  , "1 NAME Dittie /Jackson/"
  , "1 SEX F"
  , "1 FAMS @F2@"
  , "0 @F1@ FAM"
  , "1 HUSB @P2@"
  , "1 WIFE @P1@"
  , "1 CHIL @P3@"
  , "2 _FREL Natural"
  , "2 _MREL Natural"
  , "0 @F2@ FAM"
  , "1 HUSB @P3@"
  , "1 WIFE @P4@"
  , "1 CHIL @P5@"
  , "2 _FREL Natural"
  , "2 _MREL Natural"
  , "0 TRLR"
  ]

{- | A node starts with an expression that has 0 as its line 
     sequence number. A node has 0 or more properties. Those 
     are numbered too. If the next line's sequence number is 
     higher than the current one's, then the current property 
     owns the higher numbered property. If the next line's 
     sequence number is the same, then it indicates a property 
     owned by the same as its predecessor. If the next line's 
     sequence number is lower, then it indicates a new property 
     adjacent to the parent of the current. This method tests 
     whether the parser can morph a simple document into the 
     correct amount of nodes. 
-}
canMorphValidSmallDocIntoNodes :: Bool
canMorphValidSmallDocIntoNodes = outcome where
  input = produceValidDoc9Branches
  parsed = Parser.parse input
  tree = Parser.toTree parsed
  count = length tree
  expectedAmount = 9
  outcome = expectedAmount == count

{- | The parser must be able to turn a node property into a leaf.
     Node properties are parts of GEDExpressions. They identify
     the type of the expression as well as its value. Leaves are
     owned by branches and can own more leaves themselves. The
     function Parser.toLeaf is tested alongside the Leaf 
     constructor.
-}
canMakeLeaf :: Bool
canMakeLeaf = outcome where
  expectedValue = "Leaf"
  input = Parser.NodeProperty Parser.p_titl expectedValue 
  (Parser.Leaf t v xs) = Parser.toLeaf input
  expectedType = Parser.p_titl
  outcome = expectedType == t 
              && expectedValue == v 
              && 0 == length xs

{- | Makes sure that the parser can show a Leaf
     and that doing so returns the expected outcome.
-}
canShowLeaf :: Bool
canShowLeaf = outcome where
  leaf = Parser.toLeaf (Parser.NodeProperty Parser.p_husb "@p2@")
  shown = show leaf
  expected = "Leaf ReferenceProperty \"HUSB\" \"@p2@\""
  outcome = expected == shown

{- | The parser must be able to detect whether the property 
     described in a GEDExpression is known. If not, it could
     mean 1 of 2 options: 
     - either the parser is incomplete, or
     - the input document is malformed.
     The function under test, Parser.isNodePropertyTypeUnknown,
     cannot determine the difference.
-}
canDetectUnknownPropertyType :: Bool
canDetectUnknownPropertyType = outcome where
  input = "2 UNKN Unknown"
  maybeParsed = Testing.parseGEDLine input
  parsed = Data.Maybe.fromJust maybeParsed
  found = Parser.isNodePropertyTypeUnknown parsed
  expected = True
  outcome = expected == found

{- | The parser must be able to turn a single property into 
     a single leaf.
-}
canConvertNodePropertyToLeaf :: Bool
canConvertNodePropertyToLeaf = outcome where
  expectedType = Parser.p_wife
  expectedValue = "@p1@"
  input = Parser.NodeProperty expectedType expectedValue
  (Parser.Leaf t v sx) = Parser.toLeaf input
  outcome = expectedType == t 
              && expectedValue == v 
              && 0 == length sx

{- | The parser must be able to turn a bunch of properties 
     into branch leaves, and the properties of leaves into
     child leaves. The input is lines 2 through 7 from the
     result of produceValidDoc9Branches. It contains 5 lines.
     The last 2 leaves are properties of the 3rd. Thus the 
     length of Parser.toLeaves must be 3, with the last leaf
     having 2 child leaves.
-}
canConvertNodePropertiesToBranchLeaves :: Bool
canConvertNodePropertiesToBranchLeaves = outcome where
  input = produceValidDoc9Branches 
  parsed = Parser.parse input
  bunch = drop 1 . take 6 $ parsed
  test1 = 5 == length bunch
  leaves = Parser.toLeaves [] bunch
  test2 = 3 == length leaves
  outcome = test1 && test2

{- | The parser must be able to turn a fitting expression
     into a branch.
-}
canTurnBranchExpressionIntoBranch :: Bool
canTurnBranchExpressionIntoBranch = outcome where
  input = "0 @P3@ INDI"
  parsed = Data.Maybe.fromJust $ Parser.parseLine input 
  test1 = Parser.isBranch parsed
  shown = show $ Parser.toBranch parsed
  expected = "Branch PersonNode having: Leaf NodeIdentifierProperty INDI \"@P3@\""
  test2 = expected == shown
  outcome = test1 && test2

{- | The parser must not be able to turn a leaf expression
     into a branch. The funtion under test, Parser.isBranch,
     must not break or fail. Its output must identify the node
     as an unknown type (because it isn't a node), and must
     echo the correct value property.
-}
cannotTurnLeafExprIntoBranch :: Bool
cannotTurnLeafExprIntoBranch = outcome where
  input = "2 VERS 5.5"
  parsed = Data.Maybe.fromJust $ Parser.parseLine input
  test1 = not (Parser.isBranch parsed)
  expected2 = "2 ValuedProperty \"VERS\" \"5.5\""
  test2 = expected2 == show parsed
  shown = show $ Parser.toBranch parsed
  expected3 = "Branch UnknownNodeType having: Leaf ValuedProperty \"VERS\" \"5.5\""
  test3 = expected3 == shown
  outcome = test1 && test2 && test3

makeTree :: [Data.Text.Text] -> [Parser.Branch]
makeTree xs = t where
  p = Parser.parse xs
  t = Parser.toTree p

makeRawXml :: [Data.Text.Text] -> Data.Text.Text
makeRawXml xs = u where
  x = Writer.toXml (makeTree xs) ("", "", -1)
  u = Data.Text.unlines x

canCreateRawXml :: Bool
canCreateRawXml = outcome where 
  u = makeRawXml produceValidDoc9Branches
  expectedCount = 1318
  test1 = expectedCount == Data.Text.length u
  outcome = test1

canFilterToFindNone :: Bool
canFilterToFindNone = outcome where
  t = makeTree produceValidDoc9Branches
  f = Parser.filterTree t ("@DoesNotExist@", "", 2)
  count2 = length f
  expectedAmount2 = 9
  test2 = count2 == expectedAmount2
  outcome = test2

canFilterToFindPerson :: Bool
canFilterToFindPerson = outcome where
  t = makeTree produceValidDoc9Branches
  f = Parser.filterTree t ("@P5@", "", 0)
  count2 = length f
  expectedAmount2 = 3
  test2 = count2 == expectedAmount2
  outcome = test2

canFilterToFindGrandparents :: Bool
canFilterToFindGrandparents = outcome where
  t = makeTree produceValidDoc9Branches
  f = Parser.filterTree t ("@P5@", "", 4)
  count2 = length f
  expectedAmount2 = 9
  test2 = count2 == expectedAmount2
  outcome = test2

canMatchAndThisLeaf :: Bool
canMatchAndThisLeaf = outcome where
  expectedType = Parser.p_wife
  expectedValue = "@P7@"
  input = Parser.NodeProperty expectedType expectedValue
  leaf = Parser.toLeaf input
  outcome = Parser.matchAndThisLeaf expectedValue leaf
  
canFindEndIdInTree :: Bool
canFindEndIdInTree = outcome where
  t = makeTree produceValidDoc9Branches
  test1 = Data.Maybe.isJust (Parser.findEndNode t "@F1@")
  test2 = Data.Maybe.isJust (Parser.findEndNode t "@F2@")
  test3 = Data.Maybe.isJust (Parser.findEndNode t "@P5@")
  test4 = Data.Maybe.isJust (Parser.findEndNode t "@P3@")
  outcome = test1 && test2 && test3 && test4

canGetBranchId :: Bool
canGetBranchId = outcome where
  t = makeTree produceValidDoc9Branches
  test1 = case (Parser.findEndNode t "@F1@") of
    Nothing -> False
    Just node -> "@F1@" == Parser.getBranchId node
  test2 = case (Parser.findEndNode t "@F2@") of
    Nothing -> False
    Just node -> "@F2@" == Parser.getBranchId node
  test3 = case (Parser.findEndNode t "@P5@") of
    Nothing -> False
    Just node -> "@P5@" == Parser.getBranchId node
  test4 = case (Parser.findEndNode t "@P3@") of
    Nothing -> False
    Just node -> "@P3@" == Parser.getBranchId node
  outcome = test1 && test2 && test3 && test4

canFindParentFamily :: Bool
canFindParentFamily = outcome where
  tree = makeTree produceValidDoc9Branches
  parents = Parser.findParents tree "@P5@"
  expectedAmount = 1 
  countedAmount = length parents
  test1 = expectedAmount == countedAmount
  outcome = test1
   
canFindParentsOfFamily :: Bool
canFindParentsOfFamily = outcome where
  tree = makeTree produceValidDoc9Branches
  parents = Parser.findParents tree "@F2@"
  expectedAmount = 2 
  countedAmount = length parents
  test1 = expectedAmount == countedAmount
  outcome = test1
