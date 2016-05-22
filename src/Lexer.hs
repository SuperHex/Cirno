-- | lexem defination of Cirno.

module Lexer where

import           Control.Monad          (void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

type Name = String

data Exp
  = EInt Integer
  | Var Name
  | App Exp
        Exp
  | Lam Pattern
        Exp
  | Let [(Pattern,Exp)]
        Exp
  | Letrec [(Pattern,Exp)]
           Exp
  | Fatbar Exp
           Exp
  | If Exp
       Exp
       Exp
  | Case Name
         [Clause]
  deriving ((Show))

data Pattern
  = PInt Integer
  | PVar Name
  | PCons Name
          [Pattern]
  deriving (Show)

data Clause =
  Clause Name
         [Name]
         Exp
  deriving ((Show))

spaceConsumer :: Parser ()
spaceConsumer = L.space
                  singleSpace                       -- how to consum single whitespace
                  lineComment                       -- how to consum line comment
                  blockComment                      -- how to consum block of comment
  where singleSpace = void spaceChar
        lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

warpParens :: Parser a -> Parser a
warpParens p = parens (warpParens p) <|> p

integer :: Parser Integer
integer = lexeme L.integer

keywords :: [String]
keywords = ["if","else","then","do","case","of","return","let","letrec","->"]

keyword :: String -> Parser ()
keyword w =
  do void $ string w
     void $ notFollowedBy alphaNumChar              -- | perform longest match to make sure it is a keyword.
     spaceConsumer

identifier' :: Parser String                         -- | an identifier is string start with a lowercase char, followed by
                                                    -- | many chars, but not a reserved keyword.
identifier' = name >>= check
  where name = (:) <$> lowerChar <*> many alphaNumChar
        check x | x `elem` keywords = fail ("keyword" ++ show x ++ "is a reserved word")
                | otherwise         = return x

identifier :: Parser String
identifier = lexeme identifier'

constructor' :: Parser String                        -- | same as constructor, but do not comsume whitespace
constructor' = name
  where name = (:) <$> upperChar <*> many alphaNumChar

constructor :: Parser String
constructor = lexeme constructor'
