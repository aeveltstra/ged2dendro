{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Read

data NodeType = PersonNode
              | SourceNode
              | FamilyNode
              | HeaderNode
              | TrailerNode
              | RepositoryNode
              | UnknownNodeType
              deriving (Bounded, Enum, Eq, Read, Show)


type PropertyValue = Data.Text.Text
type PropertyName = Data.Text.Text

const_INDI, const_FAM, const_REPO, const_SOUR, const_UNKNOWN :: String
const_INDI = ['I','N','D','I'] 
const_FAM = ['F','A','M']
const_REPO = ['R','E','P','O']
const_SOUR = ['S','O','U','R']
const_UNKNOWN = ['U','N','K','N']

nodeIdentifierTypeValues :: [String]
nodeIdentifierTypeValues = [const_INDI, const_FAM, const_REPO, const_SOUR, const_UNKNOWN]

data NodeIdentifierType = IdentifiesAnIndividual
                        | IdentifiesAFamily
                        | IdentifiesARepository
                        | IdentifiesASource
                        | UnknownNodeIdentifierType
                        deriving (Bounded, Enum, Eq)

instance Show NodeIdentifierType where 
  show IdentifiesAnIndividual = const_INDI
  show IdentifiesAFamily = const_FAM
  show IdentifiesARepository = const_REPO
  show IdentifiesASource = const_SOUR
  show UnknownNodeIdentifierType = const_UNKNOWN

instance Read NodeIdentifierType where
  readsPrec _ x   
      | x == const_INDI = [(IdentifiesAnIndividual, "")]
      | x == const_FAM = [(IdentifiesAFamily, "")]
      | x == const_REPO = [(IdentifiesARepository, "")]
      | x == const_SOUR = [(IdentifiesASource, "")]
      | otherwise = [(UnknownNodeIdentifierType, x)]
{-
-- don't do this. ghc will complain this doesn't exhaust.
  readsPrec _ const_INDI = [(IdentifiesAnIndividual, "")]
  readsPrec _ const_FAM = [(IdentifiesAFamily, "")]
  readsPrec _ const_REPO = [(IdentifiesARepository, "")]
  readsPrec _ const_SOUR = [(IdentifiesASource, "")]
  readsPrec _ x = [(UnknownNodeIdentifierType, x)]
-}

data PlaceCategory = BirthPlace
                   | BurialPlace
                   | DeathPlace
                   | UnknownPlaceCategory
                   deriving (Bounded, Enum, Eq)

const_BIRT, const_BURI, const_DEAT :: String
const_BIRT = ['B','I','R','T']
const_BURI = ['B','U','R','I']
const_DEAT = ['D','E','A','T']

placeCategoryValues :: [String]
placeCategoryValues = [const_BIRT, const_BURI, const_DEAT]

instance Show PlaceCategory where 
  show BirthPlace = const_BIRT 
  show BurialPlace = const_BURI
  show DeathPlace = const_DEAT
  show _ = const_UNKNOWN

instance Read PlaceCategory where
  readsPrec _ x
    | x == const_BIRT = [(BirthPlace, "")]
    | x == const_BURI = [(BurialPlace, "")]
    | x == const_DEAT = [(DeathPlace, "")]
    | otherwise = [(UnknownPlaceCategory, x)]

data PropertyType = EmptyProperty PropertyName
              | ValuedProperty PropertyName 
              | ReferenceProperty PropertyName
              | NodeIdentifierProperty NodeIdentifierType
              | PlaceCategoryProperty PlaceCategory
              | UnknownPropertyType
              deriving (Eq, Read, Show)

indiNodeIdentifier, famNodeIdentifier, repoNodeIdentifier,
  sourNodeIdentifier, unknownNodeIdentifier :: PropertyType
indiNodeIdentifier = NodeIdentifierProperty IdentifiesAnIndividual
famNodeIdentifier = NodeIdentifierProperty IdentifiesAFamily
repoNodeIdentifier = NodeIdentifierProperty IdentifiesARepository
sourNodeIdentifier = NodeIdentifierProperty IdentifiesASource
unknownNodeIdentifier = NodeIdentifierProperty UnknownNodeIdentifierType

matches :: PropertyType -> Data.Text.Text -> Bool
matches (UnknownPropertyType) _ = False 
matches (EmptyProperty a) b = a == b
matches (ValuedProperty a) b = a == b
matches (ReferenceProperty a) b = a == b
matches (PlaceCategoryProperty a) b = show a == Data.Text.unpack b
matches _ _ = False

const_HEAD, const_TRLR :: Data.Text.Text
const_HEAD = "HEAD"
const_TRLR = "TRLR"

p_apid, p_auth, p_birt, p_buri, p_char, p_chil, p_chr, p_conc, 
  p_corp, p_data, p_date, p_deat, p_div, p_famc, p_fams, p_file, 
  p_form, p_frel, p_gedc, p_head, p_husb, p_marr, p_milt, p_mrel,
  p_name, p_note, p_obje, p_page, p_plac, p_prob, p_publ, p_repo,
  p_resi, p_sex, p_sour, p_text, p_titl, p_trlr, p_vers, 
  p_wife :: PropertyType
p_apid = ValuedProperty "_APID"
p_auth = ValuedProperty "AUTH"
p_birt = PlaceCategoryProperty BirthPlace
p_buri = PlaceCategoryProperty BurialPlace
p_char = ValuedProperty "CHAR"
p_chil = ReferenceProperty "CHIL"
p_chr  = ValuedProperty "CHR"
p_conc = ValuedProperty "CONC"
p_corp = ValuedProperty "CORP"
p_data = EmptyProperty "DATA"
p_date = ValuedProperty "DATE"
p_deat = PlaceCategoryProperty DeathPlace
p_div  = EmptyProperty "DIV"
p_famc = ReferenceProperty "FAMC"
p_fams = ReferenceProperty "FAMS"
p_file = ValuedProperty "FILE"
p_form = ValuedProperty "FORM"
p_frel = ValuedProperty "_FREL"
p_gedc = ValuedProperty "GEDC"
p_head = EmptyProperty const_HEAD
p_husb = ReferenceProperty "HUSB"
p_marr = EmptyProperty "MARR"
p_milt = EmptyProperty "_MILT"
p_mrel = ValuedProperty "_MREL"
p_name = ValuedProperty "NAME"
p_note = ValuedProperty "NOTE"
p_obje = EmptyProperty "OBJE"
p_page = ValuedProperty "PAGE"
p_plac = ValuedProperty "PLAC"
p_prob = EmptyProperty "PROB"
p_publ = ValuedProperty "PUBL"
p_repo = ReferenceProperty "REPO"
p_resi = ValuedProperty "RESI"
p_sex  = ValuedProperty "SEX"
p_sour = ReferenceProperty "SOUR"
p_text = ValuedProperty "TEXT"
p_titl = ValuedProperty "TITL"
p_trlr = EmptyProperty const_TRLR
p_vers = ValuedProperty "VERS"
p_wife = ReferenceProperty "WIFE"

propertyTypes, emptyPropertyTypes, 
  referencePropertyTypes :: [PropertyType]
propertyTypes = [p_apid, p_frel, p_milt , p_mrel, p_auth, p_birt,
                 p_buri, p_char, p_chil, p_chr , p_conc, p_corp, 
                 p_data, p_date, p_deat, p_div , p_famc, p_fams, 
                 p_file, p_form, p_gedc, p_head, p_husb, p_marr, 
                 p_name, p_note, p_obje, p_page, p_plac, p_prob, 
                 p_publ, p_repo, p_resi, p_sex , p_sour, p_text, 
                 p_titl, p_trlr, p_vers, p_wife, 
                 indiNodeIdentifier, famNodeIdentifier, 
                 repoNodeIdentifier, sourNodeIdentifier, 
                 unknownNodeIdentifier]
emptyPropertyTypes = [p_data, p_div, p_head, p_marr, p_milt, 
                      p_obje, p_prob, p_trlr]
referencePropertyTypes = [p_chil, p_famc, p_fams, p_husb, 
                          p_repo, p_sour, p_wife]

data NodeProperty = NodeProperty PropertyType PropertyValue

instance Show NodeProperty where
  show (NodeProperty t v) = Data.List.intercalate " " [show t, show v]


type GEDLine = Data.Text.Text
type GEDLineTuple = (Data.Text.Text, Data.Text.Text, Data.Text.Text)

{- | Type and implicit constructor of a GED Expression.
     This is the parsed representation of a single line
     from a valid GED document.
-}
data GEDExpression = GEDExpression Int NodeProperty

{- | Need to be able to show the expression as parsed from 
     the document, for testing purposes. Shown is the 
     expression's sequence number followed by its node property,
     separated by 1 space.
-}
instance Show GEDExpression where
  show (GEDExpression i n) = Data.List.intercalate " " [show i, show n]

{- | Constructs an empty head expression. A valid GED document
     starts with a head expression. This exists to facilitate 
     testing and creation of valid documents. This function
     produces the same as parsing a head expression from a valid
     GED document.
-}
headExpression :: GEDExpression
headExpression = GEDExpression 0 (NodeProperty (EmptyProperty "HEAD") "")

type LeafType = PropertyType
type LeafValue = PropertyValue
type BranchType = NodeType

{- | Translating properties into leaves so they can be 
     written by a writer, for instance into XML.
     Properties are sequences. Leaves are objects, where
     each property is a child.
-}
data Leaf = Leaf LeafType LeafValue [Leaf]
toLeaf :: NodeProperty -> Leaf
toLeaf (NodeProperty t v) = (Leaf t v [])

{- | Extracts the identifier from a leaf. Only possible if the leaf 
     identifie a family node or a person. In all other cases, the 
     return value is empty. The input is a Maybe Leaf because it
     is found by findFamilyLeaf which returns a Maybe.
-}
getLeafId :: Maybe Leaf -> EndId
getLeafId Nothing = ""
getLeafId (Just (Leaf (NodeIdentifierProperty IdentifiesAFamily) v _)) = v
getLeafId (Just (Leaf (NodeIdentifierProperty IdentifiesAnIndividual) v _)) = v
getLeafId (Just _) = ""

findFamilyLeaf :: [Leaf] -> Maybe Leaf
findFamilyLeaf [] = Nothing
findFamilyLeaf leaves = Data.List.find isFamilyLeaf leaves

isFamilyLeaf :: Leaf -> Bool
isFamilyLeaf (Leaf (NodeIdentifierProperty IdentifiesAFamily)  _ _) = True
isFamilyLeaf _ = False

findPersonLeaf :: [Leaf] -> Maybe Leaf
findPersonLeaf [] = Nothing
findPersonLeaf leaves = Data.List.find isPersonLeaf leaves

isPersonLeaf :: Leaf -> Bool
isPersonLeaf (Leaf (NodeIdentifierProperty IdentifiesAnIndividual) _ _) = True
isPersonLeaf _ = False

isChildLeaf :: Leaf -> Bool
isChildLeaf (Leaf (ReferenceProperty "CHIL") _ _) = True
isChildLeaf _ = False

getLeafValue :: Leaf -> Data.Text.Text
getLeafValue (Leaf _ v _) = v

{- | Need to be able to show a leaf. Will start the output
     with the word "Leaf", followed by the property type and its
     GED code, followed by a value if it exists, followed by 
     sub leaves if they exist, separated by spaces.
-}
instance Show Leaf where 
  show (Leaf t v [])   = Data.List.intercalate " " ["Leaf", show t, show v]
  show (Leaf t v xs) = 
    (Data.List.intercalate " " ["Leaf", show t, show v, "having: "])
    ++ (Data.List.intercalate ", " (fmap show xs))
  
{- Turns a list of expressions into a list of leaves.
   If subsequent expressions start with a line number higher than the 
   current, they become a child leaf. If they start with a line number
   same to the current, they become a sibling. If the line number is
   higher than the current, they start an uncle.

   Imagine a poem. If the rhyming goes ABAB, then the output of this
   function will have the same order: [A, B, A, B]. But if it goes 
   AaBb, then the output will become [A[a], B[b]].

   The input is not a poem. It is GED expressions. The method evaluates
   each expression's index number.

   The first invocation of this should pass in an empty Leaf list. 
   It will then, if necessary, recurse and reduce the list of GED 
   expressions.
-}
toLeaves :: [(Int, Leaf)] -> [GEDExpression] -> [Leaf]
toLeaves [] [] = []
toLeaves xs [] = Data.List.reverse (fmap snd xs)
toLeaves [] (b:bs) = toLeaves [(i, k)] bs where 
  (GEDExpression i p) = b
  k = toLeaf p
toLeaves ((i, (Leaf t v n)):xs) ((GEDExpression j p):[])
  | i==j = toLeaves ((j, toLeaf p) : (i, (Leaf t v n)) : xs) []
  | i<j  = toLeaves ((i, (Leaf t v (n ++ [toLeaf p]))) : xs) []
  | otherwise = undefined
toLeaves ((i, (Leaf t v n)):xs) ((GEDExpression j p):bs)
  | i==j = toLeaves ((j, toLeaf p) : (i, (Leaf t v n)) : xs) bs
  | i<j  = toLeaves ((i, (Leaf t v (n ++ [toLeaf p]))) : xs) bs
  | otherwise = undefined
  


{- | Attempts to turn a property type into a node type.
     If the input does not match any known node type,
     this function returns the UnknownNodeType.
-}
toNodeType :: PropertyType -> NodeType
toNodeType x 
  | x == indiNodeIdentifier = PersonNode
  | x == famNodeIdentifier = FamilyNode
  | x == repoNodeIdentifier = RepositoryNode
  | x == sourNodeIdentifier = SourceNode
  | x == (EmptyProperty const_HEAD) = HeaderNode
  | x == (EmptyProperty const_TRLR) = TrailerNode
  | otherwise = UnknownNodeType

{- | Turning lines from a GED document into branches.
     The lines are sequenced in the same order as in the 
     input document. Branches are objects that can have
     properties (a.k.a. leaves).
-}
data Branch = Branch BranchType [Leaf]
toBranch :: GEDExpression -> Branch
toBranch (GEDExpression i p) =
  Branch t [(toLeaf p)] where 
  t = toNodeType (getPropertyType (GEDExpression i p))

{- | Extracts the identifier of a branch if it identifies a family
     or a person. Returns empty in all other cases.
-}
getBranchId :: Branch -> EndId
getBranchId (Branch FamilyNode leaves) = getLeafId (findFamilyLeaf leaves)
getBranchId (Branch PersonNode leaves) = getLeafId (findPersonLeaf leaves)
getBranchId _ = ""

{- | Need to be able to show the branch for testing
     purposes. The output will start with the word "Branch"
     followed by the branch type and all leaves, if any.
-}
instance Show Branch where
  show (Branch t []) = "Branch " ++ show t
  show (Branch t xs) = 
    (Data.List.intercalate " " ["Branch", show t, "having: "]) 
    ++ (Data.List.intercalate ", " (fmap show xs))

{- | Need to be able to define a sort order for each branch 
     so it can be identified uniquely in a set collection.
-}
instance Ord Branch where
  a `compare` b = (show a) `compare` (show b)

{- | There's probably a cheaper way to do this than to basically
 -   perform a deepseq, but this works and is quick to program.
 -}
instance Eq Branch where
  a == b = (show a) == (show b)

{- | Adding leaves to a branch. Provide the branch and the 
     leaves to add.
-}
addLeaves :: Branch -> [Leaf] -> Branch
addLeaves b [] = b
addLeaves (Branch t l) l2 = Branch t (l++l2)

emptyPropertyValue :: Data.Text.Text
emptyPropertyValue = ""

parseInt :: (Integral a, Bounded a, Ord a) => Data.Text.Text -> a
parseInt "" = error ("Error in Parser.parseInt: input should not be empty.")
parseInt s = n where
  n = case Data.Text.Read.signed Data.Text.Read.decimal s of
      Right (i, _) -> i
      _ -> error ("Error in Parser.parseInt: this input could not be parsed to an integer: " ++ (show s))

parseLineIndex :: [Data.Text.Text] -> Int
parseLineIndex [] = error ("Error in Parser.parseLineIndex: input should not be empty.")
parseLineIndex them = parseInt (them!!0) 

{- | Determines whether the input starts with an @. 
     This is needed to identify whether a line should
     become a new branch.
-}
startsWithAt :: Data.Text.Text -> Bool
startsWithAt x = "@" == Data.Text.take 1 x

{- | An implementation of length that doesn't evaluate the
 -   entire list (which may take long or fail for infinites)
 -   but instead determines whether the list contains at 
 -   least the amount of specified elements, and then stops.
 -   @see "https://wiki.haskell.org/Haskell_programming_tips"
 -}
holdsAtLeast :: Int -> [a] -> Bool
holdsAtLeast 0 _ = True
holdsAtLeast _ [] = False
holdsAtLeast n (_:xs) = holdsAtLeast (n-1) xs

{- | Attempts to extract the value of a GED document line,
     where that line is broken into words at its spaces.
     The 3rd and following words are the value.
-}
parseLinePropValue :: [Data.Text.Text] -> PropertyValue
parseLinePropValue [] = "" 
parseLinePropValue them 
  | (holdsAtLeast 2 them) && (startsWithAt (them!!1)) && (any (== Data.Text.unpack (them!!2)) nodeIdentifierTypeValues) = them!!1 
  | (holdsAtLeast 2 them) = Data.Text.intercalate " " (drop 2 them)
  | otherwise = ""

{- | Attempts to find the property type of a tuple
     created from the first words of a GED document line.
     The 1st word is a number, the 2nd is an identifier,
     and the 3rd is an optional value. If the number = 0,
     then the line starts a node, and the identifier says
     what kind of node is started. Otherwise the line
     does not start a node but a property, and the identif-
     ier says what type of property is started.
-}
lookupPropertyType :: GEDLineTuple -> PropertyType
lookupPropertyType ("", "", "") = UnknownPropertyType
lookupPropertyType (a, b, c)
  | ("0" == a) && (startsWithAt b) && (c == "INDI") = indiNodeIdentifier
  | ("0" == a) && (startsWithAt b) && (c == "FAM") = famNodeIdentifier
  | ("0" == a) && (startsWithAt b) && (c == "REPO") = repoNodeIdentifier
  | ("0" == a) && (startsWithAt b) && (c == "SOUR") = sourNodeIdentifier
  | ("0" == a) && (startsWithAt b) = unknownNodeIdentifier
  | otherwise = n where 
      n = case (Data.List.find (\y -> matches y b) propertyTypes) of
        Just d -> d
        _ -> UnknownPropertyType

{- | Attempts to find the property type of a GED document line
     expression that got split by space.
     If no type was found, returns UnknownPropertyType.
-}
parseLinePropType :: [Data.Text.Text] -> PropertyType
parseLinePropType [] = UnknownPropertyType
parseLinePropType them
  | holdsAtLeast 2 them = lookupPropertyType (them!!0, them!!1, them!!2)
  | otherwise = UnknownPropertyType


{- | Turns a single document line into a semantically
     significant data instance. After this step,
     use toTree() to create Branch instances. They can
     be morphed into XML elements.
-}
parseLine :: GEDLine -> Maybe GEDExpression
parseLine x = result where
  stripped = Data.Text.strip x
  parted  = Data.Text.splitOn " " stripped
  isLongEnough = holdsAtLeast 2 parted
  result = case isLongEnough of
           False -> Nothing
           True  -> Just (GEDExpression 
                           (parseLineIndex parted)
                           (NodeProperty
                             (parseLinePropType parted)
                             (parseLinePropValue parted)
                           )
                        )

{- | Turns a batch of document lines into semantically
     significant data instances. After this step,
     use toTree() to create Branch instances. They can
     be morphed into XML elements. Internally uses
     the parseLine() function.
-}
parse :: [GEDLine] -> [GEDExpression]
parse gedLines = a where
  a = fmap Data.Maybe.fromJust b
  b = filter Data.Maybe.isJust c
  c = fmap parseLine gedLines

{- | Extracts the property type from the passed-in expression.
     The property type helps determine how to handle the
     expression when morphing it later.
-}
getPropertyType :: GEDExpression -> PropertyType
getPropertyType (GEDExpression _ (NodeProperty b _)) = b

{- | Whether the parser does not yet know the property that
     is defined in the passed-in expression. This is useful
     to test whether the parser is complete.
-}
isNodePropertyTypeUnknown :: GEDExpression -> Bool
isNodePropertyTypeUnknown x = r where
  q = getPropertyType x
  r = UnknownPropertyType == q

{- | Whether the parser does not yet know the property that
     is defined in the passed-in expressions. This is useful
     to test whether the parser is complete.
-}
hasUnknownProperty :: [GEDExpression] -> Bool
hasUnknownProperty [] = False
hasUnknownProperty them = Data.List.any isNodePropertyTypeUnknown them

{- | Whether the passed-in expression is the start of a Branch.
     Branches are objects that can have properties. They can be
     morphed into XML elements.
-}
isBranch :: GEDExpression -> Bool
isBranch (GEDExpression i _) = i == 0

{- | Whether the passed-in expression is a document header. -}
isHeader :: GEDExpression -> Bool
isHeader (GEDExpression 0 (NodeProperty j _)) = p_head == j
isHeader _ = False

{- | Counts how many branches a document should create. 
 -   Use with care. This will cause the entire list to get
 -   evaluated. For infinite lists this will fail.
 -}
countBranches :: [GEDExpression] -> Int
countBranches [] = 0
countBranches parsed = n where 
  f = filter (isBranch) parsed 
  n = length f

{- | Folds a document of expression lines into branches. Each
     branch owns 0 or more leaves. These can be turned into XML
     elements by Writer. You can create the input by applying
     Parser.parse() to the lines of an input GED document.
     Internally uses Parser.toBranches(), starting with an empty
     list. A valid document starts with a header, so that is
     checked first. If that check fails, an error is thrown.
     Output is a list of branches.
-}
toTree :: [GEDExpression] -> [Branch]
toTree xs = do
  if isHeader (head xs) 
     then toBranches [] xs
     else error "Input invalid. Must start with header expression."

{- | A branch is a GEDExpression with index 0, that owns all
     subsequent GEDExpressions up to but excluding the next
     one with index 0. The non-0 expressions will become 
     leaves. Each branch can own 0 or more leaves. Each leaf
     can own 0 or more leaves. Input typically starts with an 
     empty list of branches and a filled list of GEDExpressions.
     Output is a list of branches. These can be turned into 
     XML elements by Writer.
-}
toBranches :: [Branch] -> [GEDExpression] -> [Branch]
toBranches tree [] = tree
toBranches tree xs = Data.List.reverse z where 
  b = break (isBranch) xs
  c = fst b
  d = snd b
  z = case (null c) of
        True -> toBranches ((toBranch (head d)):tree) (tail d)
        False -> toBranches ((addLeaves (head tree) (toLeaves [] c)):(tail tree)) d

type EndId = Data.Text.Text
type AndThis = Data.Text.Text
type MaxDepth = Integer

{- | Extracts and returns all child identifiers from family branches. -}
getFamilyChildren :: Branch -> [EndId]
getFamilyChildren (Branch FamilyNode leaves) 
  = fmap getLeafValue (Data.List.filter isChildLeaf leaves)
getFamilyChildren _ = []

{- | Returns all the branches that are children of the ancestors,
     that fit the width and depth limitations.
     If the ancestors or tree are empty, the return is empty.
     If the depth equals -1, it is ignored.

     Parameters:
     1 The ancestors of whom to find their children.
     2 (Part of) the tree as built by Parser.toTree.
     3 The max amount of child generations to return.
-}
findChildren :: [Branch] -> [Branch] -> MaxDepth -> [Branch]
findChildren [] _  _ = []
findChildren _  [] _ = []
findChildren _  _  0 = []
findChildren (parent:parents) tree depth
  = (Data.List.filter (\branch -> 
       Data.List.any (\childId ->
          childId == getBranchId branch
       ) (getFamilyChildren parent) 
    ) tree)
    ++ (findChildren parents tree depth)

{- | Limits the tree to fit the shape of the passed-in limitations.
     - EndId should specify the person or family with which to end 
       the tree. If not found, the filter will return the entire tree.
     - AndThis should specify any text to identify which other 
       nodes (people, families, resources, etc.) to include.
     - MaxDepth should specify how many generations of ancestors 
       to include upwards from the person or family identified as EndId. Double the number you expect.
     See filterEndNode.
     The output will always include the input tree's header and trailer
     branches, if they exist.
-}
filterTree :: [Branch] -> (EndId, AndThis, MaxDepth) -> [Branch]
filterTree [] _ = []
filterTree tree (endId, andThis, maxDepth) = q where
  endNodes  = (findEndNodes tree endId) ++ (findAdditions tree andThis)
  ancestors = findAncestors tree endNodes maxDepth
  q = case null endNodes of 
        True -> tree
        False -> (findHeaderBranches tree) 
               ++ distinct ( 
                   ancestors
                   ++ endNodes
                   ++ (findChildren (endNodes ++ ancestors) tree maxDepth)
                 )
               ++ (findTrailerBranches tree)
             
{- | Removes duplicates. -}
distinct :: [Branch] -> [Branch]
distinct tree = Data.Set.toList (Data.Set.fromList tree)

{- | Whether the passed-in identifier identifies the leaf if that is a child. -}
matchesChildLeafId :: EndId -> Leaf -> Bool
matchesChildLeafId "" _ = False
matchesChildLeafId i (Leaf (ReferenceProperty "CHIL") v _) = v == i
matchesChildLeafId _ _ = False

{- | A leaf is the requested child if its property type is CHILn 
     and its value equals the passed-in identifier.
-}
isAnyLeafThisChild :: [Leaf] -> EndId -> Bool
isAnyLeafThisChild [] _ = False
isAnyLeafThisChild _ "" = False
isAnyLeafThisChild leaves i = Data.List.any (matchesChildLeafId i) leaves

{- | Whether the passed-in identifier identifies the leaf if that is a parent family. -}
matchesFamilyLeafId :: EndId -> Leaf -> Bool
matchesFamilyLeafId "" _ = False
matchesFamilyLeafId i (Leaf (ReferenceProperty "FAMS") v _) = v == i
matchesFamilyLeafId _ _ = False

{- | A leaf is the requested family if its property type is a parent 
     family and its value equals the passed-in identifier.
-}
isAnyLeafThisFamily :: [Leaf] -> EndId -> Bool
isAnyLeafThisFamily [] _ = False
isAnyLeafThisFamily _ "" = False
isAnyLeafThisFamily leaves i = Data.List.any (matchesFamilyLeafId i) leaves

{- | A branch is a parent of the passed-in identifier if:
     - the branch is a family node, and
     - one or more of its leaves is a child identifier whose 
       identity equals the passed-in identifier.
-}
isParent :: Branch -> EndId -> Bool
isParent _ "" = False
isParent (Branch FamilyNode leaves) i = (isAnyLeafThisChild leaves i)
isParent (Branch PersonNode leaves) i = (isAnyLeafThisFamily leaves i)
isParent _ _ = False

{- | Locates and returns all branches in the tree that are listed
     as the parent of the passed-in child. Typically that includes
     person and family nodes, only.
     See isParent.

     Parameters:
     1. (Part of) the tree as built by Parser.toTree. If this is 
        empty, the return is empty.
     2. The End Identifier that specifies the person or family
        whose parents need to get found.
-}
findParents :: [Branch] -> EndId -> [Branch]
findParents [] _ = []
findParents tree "" = tree
findParents (x:xs) endId = b where
  b = case (isParent x endId) of
        False -> findParents xs endId
        True  -> x : (findParents xs endId)

{- | Locates and returns those branches that are ancestors
     of the person or family identified by EndId, to a maximum
     amount of generations upwards as specified by MaxDepth.
     If EndId is empty or MaxDepth equals -1, the entire tree
     will be returned. If MaxDepth equals 0, the return is empty.
     If the input tree is empty, the return will be empty.
     See findParents, fa2.

     Parameters:
     1. (Part of) the tree built from the GED document by 
        Parser.toTree. If this is empty, the return is empty.
     2. End Nodes: person nodes and / or family nodes, of whom
        to find the ancestors. If this is empty, the return is
        too.
     3. Maximum amount of ancestral generations to return.
        Note that GED documents list families and individuals
        separately, so you need to double the amount of 
        generations you expect. If this equals 0, no more
        branches get returned. If it equals -1, all branches
        get returned.
-}
findAncestors :: [Branch] -> [Branch] -> MaxDepth -> [Branch]
findAncestors []   _  _    = []
findAncestors _    _  0    = []
findAncestors tree _ (-1)  = tree
findAncestors _    [] _    = []
findAncestors tree (child:children) depth = parents ++ ancestors ++ others where
  parents = findParents tree (getBranchId child)
  ancestors = fa2 parents tree depth
  others = findAncestors tree children depth

{- | Locates the parents of the parents until there are none left,
     or until the maxDepth is depleted. Typically called by
     findAncestors and itself, recursively. 
     See findParents.

     Parameters:
     1. Parent branches found by findParents. If this is empty,
        the return is empty.
     2. (Part of) the tree as built by Parser.toTree. If this is 
        empty, the return is empty.
     3. Amount of generations of ancestors to go up.
        Note: GED trees have families separate from individuals.
        That means that if you expect 1 generation, you need to
        specify 2. If this is 0 or less, the result is empty.
-}
fa2 :: [Branch] -> [Branch] -> MaxDepth -> [Branch]
fa2 [] _ _ = []
fa2 _ [] _ = []
fa2 _  _ 0 = []
fa2 (child:children) tree maxDepth  
  | 0 >= maxDepth = []
  | otherwise = parents ++ grandparents ++ others where 
      parents = findParents tree (getBranchId child)
      grandparents = fa2 parents tree (maxDepth - 1)
      others = fa2 children tree maxDepth

{- | Returns all branches that are a header. -}
findHeaderBranches :: [Branch] -> [Branch]
findHeaderBranches [] = []
findHeaderBranches tree = Data.List.filter isHeaderBranch tree

{- | Whether the passed-in branch is a header. -}
isHeaderBranch :: Branch -> Bool
isHeaderBranch (Branch HeaderNode _) = True
isHeaderBranch _ = False

{- | Returns all branches that are a trailer. -}
findTrailerBranches :: [Branch] -> [Branch]
findTrailerBranches [] = []
findTrailerBranches tree = Data.List.filter isTrailerBranch tree

{- | Whether the passed-in branch is a trailer. -}
isTrailerBranch :: Branch -> Bool
isTrailerBranch (Branch TrailerNode _) = True
isTrailerBranch _ = False

{- | Attempts to find the first branch that has the passed-in EndId.
     See matchBranchId.
-}
findEndNode :: [Branch] -> EndId -> Maybe Branch
findEndNode tree endId = Data.List.find (matchBranchId endId) tree

{- | Finds all branches that have the passed-in EndId.
     See matchBranchId.
-}
findEndNodes :: [Branch] -> EndId -> [Branch]
findEndNodes []   _     = []
findEndNodes _    ""    = []
findEndNodes tree endId = Data.List.filter (matchBranchId endId) tree

{- | Finds all branches that have the passed-in text.
     See matchBranchId.
-}
findAdditions :: [Branch] -> AndThis -> [Branch]
findAdditions []   _       = []
findAdditions _    ""      = []
findAdditions tree andThis = Data.List.filter (matchAndThisBranch andThis) tree

{- | Whether the passed-in EndId matches the passed-in Branch.
     This can only ever be true if the branch is either a family node 
     or a person node.
     See getLeafId, findFamilyLeaf, findPersonLeaf.
-}
matchBranchId :: EndId -> Branch -> Bool
matchBranchId "" _ = False
matchBranchId i (Branch FamilyNode leaves) = isFound where
  isFound = i == getLeafId (findFamilyLeaf leaves)
matchBranchId i (Branch PersonNode leaves) = isFound where
  isFound = i == getLeafId (findPersonLeaf leaves)
matchBranchId _ _ = False


{- | Whether the passed-in text matches the value any of the 
 -   passed-in branch' leaves.
-}
matchAndThisBranch :: AndThis -> Branch -> Bool
matchAndThisBranch "" _ = False
matchAndThisBranch andThis (Branch _ leaves) = 
  Data.List.any (matchAndThisLeaf andThis) leaves


{- | Whether the passed-in text matches the value of the passed-in leaf
 -   or any of its child leaves.
-}
matchAndThisLeaf :: AndThis -> Leaf -> Bool
matchAndThisLeaf "" _ = False
matchAndThisLeaf andThis (Leaf _ value leaves) = 
  (Data.Text.isInfixOf andThis value) 
  || (Data.List.any (matchAndThisLeaf andThis) leaves)

