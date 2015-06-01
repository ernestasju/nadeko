module Navision where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Control.Applicative ((<$), (<*))
import Data.Char (isHexDigit)

-- * Navision application datatypes.

data CodeunitObject = CodeunitObject { objectId :: Int,
                                       objectName :: String,
                                       objectProperties :: [Property],
                                       properties :: [Property],
                                       code :: String
                                       } deriving (Show)

data MenuSuiteObject = MenuSuiteObject { menuSuiteId         :: Int,
                                         menuSuiteName       :: String,
                                         menuSuiteObjectProperties :: [Property],
                                         menusuiteProperties :: (),
                                         menuNodes           :: [MenuNode]
                                       } deriving (Show)

data Property = CaptionML [MultiLanguage]
              | CFRONTMayUsePermissions Bool
              | Date String
              | Deleted Bool
              | DepartmentCategory DepartmentCategoryType
              | Enabled Bool
              | FirstChild GUID
              | Image Int
              | IsDepartmentPage Bool
              | IsShortcut Bool
              | MemberOfMenu GUID
              | Modified Bool
              | Name String
              | NextNodeID GUID
              | OnRun String
              | ParentNodeID GUID
              | Permissions [TableDataPermission]
              | RunObjectID Int
              | RunObjectType RunObjectType
              | SingleInstance Bool
              | Time String
              | TableNo Int
              | VersionList String
              | Visible Bool
                deriving (Show)

data MultiLanguage = MultiLanguage String String deriving (Show)
data TableDataPermission = TableDataPermission Int [TableDataPermissionFlag] deriving (Eq, Show)
data MenuNode = MenuNode MenuNodeType GUID [Property] deriving (Show)

data RunObjectType = Codeunit
                   | DataPort
                   | Form
                   | Page
                   | Report
                   | Query
                   | XMLport
                     deriving (Show)

data DepartmentCategoryType = Administration
                            | Documents
                            | History
                            | Lists
                            | ReportsAndAnalysis
                            | Tasks deriving (Show)

data TableDataPermissionFlag = Read
                             | Insert
                             | Modify
                             | Delete
                               deriving (Eq, Show)

data MenuNodeType = Root | Menu | MenuGroup | MenuItem deriving (Show)


-- * GUID datatype.

data GUID = GUID String String String String String
          | Invalid
            deriving (Show)

-- | Smart type constructor for GUID datatype.
guid :: String -> String -> String -> String -> String -> GUID
guid a@[_, _, _, _, _, _, _, _]
     b@[_, _, _, _]
     c@[_, _, _, _]
     d@[_, _, _, _]
     e@[_, _, _, _, _, _, _, _, _, _, _, _]
     | all (all isHexDigit) [a, b, c, d, e] = GUID a b c d e
guid _ _ _ _ _ = Invalid


-- * Text format parser implementation using Parsec.

parseGUID :: Parser GUID
parseGUID = lexeme $ between (string "[{") (string "}]") $ do
    xs1 <- count 8 hexDigit
    _ <- char '-'
    xs2 <- count 4 hexDigit
    _ <- char '-'
    xs3 <- count 4 hexDigit
    _ <- char '-'
    xs4 <- count 4 hexDigit
    _ <- char '-'
    xs5 <- count 12 hexDigit
    return $ guid xs1 xs2 xs3 xs4 xs5

parseObjectProperty :: Parser Property
parseObjectProperty =
        parseProperty Date        "Date"         (parseBracketString <|> parsePropertyString)
    <|> parseProperty Modified    "Modified"     parseYesNo
    <|> parseProperty Time        "Time"         (parseBracketString <|> parsePropertyString)
    <|> parseProperty VersionList "Version List" (parseBracketString <|> parsePropertyString)

parseRunObjectType :: Parser RunObjectType
parseRunObjectType =
        parseEnum Codeunit "Codeunit"
    <|> parseEnum DataPort "DataPort"
    <|> parseEnum Form     "Form"
    <|> parseEnum Page     "Page"
    <|> parseEnum Query    "Query"
    <|> parseEnum Report   "Report"
    <|> parseEnum XMLport  "XMLport"

parseDepartymentCategoryType :: Parser DepartmentCategoryType
parseDepartymentCategoryType =
        parseEnum ReportsAndAnalysis "Reports and Analysis"
    <|> parseEnum Lists              "Lists"
    <|> parseEnum Tasks              "Tasks"
    <|> parseEnum Documents          "Documents"
    <|> parseEnum Administration     "Administration"
    <|> parseEnum History            "History"

parseTableDataPermissions :: Parser [TableDataPermission]
parseTableDataPermissions = parseTableDataPermission `sepBy` (char ',' >> spaces)

parseTableDataPermission :: Parser TableDataPermission
parseTableDataPermission = do
    _ <- lexeme $ string "TableData"
    n <- parseInt
    _ <- lexeme $ char '='
    xs <- many parseTableDataPermissionFlag
    return $ TableDataPermission n xs

parseTableDataPermissionFlag :: Parser TableDataPermissionFlag
parseTableDataPermissionFlag =
        (Read   <$ char 'r')
    <|> (Insert <$ char 'i')
    <|> (Modify <$ char 'm')
    <|> (Delete <$ char 'd')

parseMultiLanguage :: Parser [MultiLanguage]
parseMultiLanguage = choice [between (char '[') (char ']') (sepEndBy parseSingleMultiLanguage (lexeme $ char ';')),
                             parseSingleMultiLanguage >>= \c -> return [c],
                             spaces >> return []]

parseSingleMultiLanguage :: Parser MultiLanguage
parseSingleMultiLanguage = do
    ln <- many1 letter <|> string "@@@"
    _ <- lexeme $ char '='
    tr <- parseMultiLanguageString
    return $ MultiLanguage ln tr

parseMultiLanguageString :: Parser String
parseMultiLanguageString = manyTill anyChar (try (spaces >> lookAhead (oneOf ";]")))

parsePlainObjectName :: Parser String
parsePlainObjectName = manyTill anyChar (try (spaces >> lookAhead (char '{')))

parseSection :: Parser a -> Parser a
parseSection = between (lexeme $ char '{') (lexeme $ char '}')

parseProperty :: (a -> b) -> String -> Parser a -> Parser b
parseProperty c n p = liftM c (lexeme (string n) >> lexeme (char '=') >> p)

parseEnum :: a -> String -> Parser a
parseEnum c s = lexeme (string s) >> return c

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parseString :: Char -> Char -> Parser String
parseString open close = lexeme $ between (char open)
                                          (char close)
                                          (many (try (close <$ string [close, close]) <|> noneOf [close]))

parseBracketString :: Parser String
parseBracketString = parseString '[' ']'

parsePropertyString :: Parser String
parsePropertyString = manyTill anyChar (try (spaces >> lookAhead (oneOf ";}")))

parseYesNo :: Parser Bool
parseYesNo = choice [False <$ lexeme (string "No" ),
                     True  <$ lexeme (string "Yes")]

parseInt :: Parser Int
parseInt = liftM read (lexeme $ many1 digit)

parseCodeunitProperty :: Parser Property
parseCodeunitProperty =
        parseProperty CFRONTMayUsePermissions "CFRONTMayUsePermissions" parseYesNo
    <|> parseProperty OnRun                   "OnRun"                   (return "")  -- TODO: parseAnonymousProcedure
    <|> parseProperty Permissions             "Permissions"             parseTableDataPermissions
    <|> parseProperty TableNo                 "TableNo"                 parseInt
    <|> parseProperty SingleInstance          "SingleInstance"          parseYesNo

parseCodeunit :: Parser CodeunitObject
parseCodeunit = do
    _ <- lexeme $ string "OBJECT"
    _ <- lexeme $ string "Codeunit"
    i <- liftM read (lexeme $ many1 digit)
    n <- parseBracketString <|> parsePlainObjectName
    (ps, cps, c) <- parseSection $ do
                        ps  <- lexeme (string "OBJECT-PROPERTIES")
                            >> parseSection (sepEndBy (try parseObjectProperty) (lexeme $ oneOf ";}"))
                        cps <- lexeme $ string "PROPERTIES"
                            >> parseSection (sepEndBy (try parseCodeunitProperty) (lexeme $ oneOf ";}"))
                        c   <- lexeme $ string "CODE"
                            >> parseSection (return "")  -- TODO: parseCode
                        return (ps, cps, c)
    return $ CodeunitObject i n ps cps c

parseMenuSuite :: Parser MenuSuiteObject
parseMenuSuite = do
    _ <- lexeme $ string "OBJECT"
    _ <- lexeme $ string "MenuSuite"
    i <- liftM read (lexeme $ many1 digit)
    n <- parseBracketString <|> parsePlainObjectName
    (ps, mps, ns) <- parseSection $ do
                        ps  <- lexeme $ string "OBJECT-PROPERTIES"
                            >> parseSection (sepEndBy (try parseObjectProperty) (lexeme $ oneOf ";}"))
                        mps <- lexeme $ string "PROPERTIES"
                            >> parseSection (return ())
                        ns  <- lexeme $ string "MENUNODES"
                            >> parseSection (many parseMenuNode)
                        return (ps, mps, ns)
    return $ MenuSuiteObject i n ps mps ns

parseMenuNode :: Parser MenuNode
parseMenuNode = parseSection $ do
    t <- parseMenuNodeType
    _ <- lexeme $ char ';'
    g <- parseGUID
    ps <- option [] (lexeme (char ';') >> sepEndBy parseMenuNodeProperty (lexeme $ char ';'))
    return $ MenuNode t g ps

parseMenuNodeProperty :: Parser Property
parseMenuNodeProperty =
             parseProperty CaptionML          "CaptionML"          parseMultiLanguage
    <|> try (parseProperty Deleted            "Deleted"            parseYesNo)
    <|>      parseProperty DepartmentCategory "DepartmentCategory" parseDepartymentCategoryType
    <|>      parseProperty Enabled            "Enabled"            parseYesNo
    <|>      parseProperty FirstChild         "FirstChild"         parseGUID
    <|> try (parseProperty Image              "Image"              parseInt)
    <|> try (parseProperty IsDepartmentPage   "IsDepartmentPage"   parseYesNo)
    <|>      parseProperty IsShortcut         "IsShortcut"         parseYesNo
    <|>      parseProperty MemberOfMenu       "MemberOfMenu"       parseGUID
    <|> try (parseProperty Name               "Name"               (parseBracketString <|> parsePropertyString))
    <|>      parseProperty NextNodeID         "NextNodeID"         parseGUID
    <|>      parseProperty ParentNodeID       "ParentNodeID"       parseGUID
    <|> try (parseProperty RunObjectID        "RunObjectID"        parseInt)
    <|>      parseProperty RunObjectType      "RunObjectType"      parseRunObjectType
    <|>      parseProperty Visible            "Visible"            parseYesNo

parseMenuNodeType :: Parser MenuNodeType
parseMenuNodeType =
        try (parseEnum MenuGroup "MenuGroup")
    <|> try (parseEnum MenuItem  "MenuItem")
    <|>      parseEnum Menu      "Menu"
    <|>      parseEnum Root      "Root"