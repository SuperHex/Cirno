-- | helper functions for parsing Cirno

module Parser where

import           Control.Monad
import           Lexer
import           Text.Megaparsec
import           Text.Megaparsec.String

warpParser :: Parser Exp
warpParser = spaceConsumer *> expression <* eof

expression :: Parser Exp
expression =  warpParens
           $  parseIf
          <|> parseCase
          <|> parseLambda
          <|> parseLet
          <|> parseLetrec
          <|> parseApplication
          <|> parseVariable
          <|> parseConstant

pattern :: Parser Pattern
pattern =  warpParens
        $  parseIntP
       <|> parseVarP
       <|> parseConsP

parseIf :: Parser Exp
parseIf =
  do void $ keyword "if"
     cond <- warpParens expression
     void $ keyword "then"
     exp1 <- warpParens expression
     void $ keyword "else"
     exp2 <- warpParens expression
     return $ If cond exp1 exp2

parseCase :: Parser Exp
parseCase = undefined

parseLambda :: Parser Exp
parseLambda =
  do void $ keyword "\\"
     name <- warpParens identifier
     void $ keyword "."
     expr <- warpParens expression
     return $ Lam (PVar name) expr

parseLet :: Parser Exp
parseLet = undefined

parseLetrec :: Parser Exp
parseLetrec = undefined

parseApplication :: Parser Exp
parseApplication = undefined

parseVariable :: Parser Exp
parseVariable = Var <$> warpParens identifier

parseConstant :: Parser Exp
parseConstant = EInt <$> warpParens integer

parseIntP :: Parser Pattern
parseIntP = PInt <$> warpParens integer

parseVarP :: Parser Pattern
parseVarP = PVar <$> warpParens identifier

parseConsP :: Parser Pattern
parseConsP =
  do cons <- constructor
     var  <- many (warpParens pattern)
     return $ PCons cons var
