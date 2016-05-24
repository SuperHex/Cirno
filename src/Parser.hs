-- | helper functions for parsing Cirno

module Parser where

import           Control.Monad
import           Lexer
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String

warpParser :: Parser Expr
warpParser = spaceConsumer *> expression <* eof

expression :: Parser Expr
expression =  warpParens
           $  parseApplication
          <|> parseIf
          <|> parseCase
          <|> parseLambda
          <|> try parseLet
          <|> parseLetrec
          <|> parseVariable
          <|> parseConstant

definition :: Parser Definition
definition =  warpParens
           $  parseDVar
          <|> parseDFun

pattern :: Parser Pattern
pattern =  warpParens
        $  parseIntP
       <|> parseVarP
       <|> parseConsP

parseIf :: Parser Expr
parseIf =
  do void $ keyword "if"
     cond <- warpParens expression
     void $ keyword "then"
     exp1 <- warpParens expression
     void $ keyword "else"
     exp2 <- warpParens expression
     return $ If cond exp1 exp2

parseCase :: Parser Expr
parseCase =
  do void $ keyword "case"
     var  <- warpParens expression
     void $ keyword "of"
     defs <- some $ parseClauses "->"
     let (p, e) = unzip defs
         clause = zipWith Clause p e
      in return $ Case var clause

parseLambda :: Parser Expr
parseLambda =
  do void $ symbol "\\"
     name <- warpParens identifier
     void $ symbol "."
     expr <- warpParens expression
     return $ Lam (PVar name) expr

parseLet :: Parser Expr
parseLet =
  do void $ keyword "let"
     defs <- some $ parseClauses "="
     void $ keyword "in"
     expr <- warpParens expression
     let (p, e) = unzip defs
      in return $ Let p e expr

parseClauses :: String -> Parser (Pattern, Expr)
parseClauses s = try $
             do patt <- pattern
                void $ symbol s
                expr <- expression
                return (patt, expr)

parseLetrec :: Parser Expr
parseLetrec =
  do void $ keyword "letrec"
     defs <- some $ parseClauses "="
     void $ keyword "in"
     expr <- warpParens expression
     let (p, e) = unzip defs
      in return $ Letrec p e expr

parseApplication :: Parser Expr
parseApplication = makeExprParser expr sep
  where sep = [ [InfixL ((symbol "$") >> return App)]
              , [Prefix (symbol "-" >> return (App (Op "Neg")))]
              , [InfixL (symbol "*" >> return (\a b -> App (App (Op "Mul") a) b))
                ,InfixL (symbol "/" >> return (\a b -> App (App (Op "Div") a) b))]
              , [InfixL (symbol "+" >> return (\a b -> App (App (Op "Add") a) b))
                ,InfixL (symbol "-" >> return (\a b -> App (App (Op "Mul") a) b))]]
        expr =  parseIf
            <|> parseCase
            <|> parseLambda
            <|> try parseLet
            <|> parseLetrec
            <|> parseVariable
            <|> parseConstant

parseVariable :: Parser Expr
parseVariable =
  do var <- warpParens identifier
     (notFollowedBy expression >> return (Var var))

parseConstant :: Parser Expr
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

parseDVar :: Parser Definition
parseDVar = try $
  do var <- warpParens identifier
     void $ symbol "="
     expr <- expression
     return $ DVar var expr

parseDFun :: Parser Definition
parseDFun = try $
  do func <- warpParens identifier
     vars <- many identifier
     void $ symbol "="
     expr <- expression
     return $ DFun func vars expr
