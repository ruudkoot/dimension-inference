module SystemF.Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Prim

import SystemF.Inference

import qualified Data.Map as Map
import Control.Applicative hiding ((<|>))

type Parser = Parsec String ()

--Definitions for the lexer created by parsec, see parsec documentation 2.8/2.9 and refernce guide
systemFDef::LanguageDef a
systemFDef
 = LanguageDef
   { commentStart   = "{-"
   , commentEnd     = "-}"
   , commentLine    = "--"
   , nestedComments = True
   , identStart     = letter
   , identLetter    = alphaNum <|> char '-' <|> char '_'
   , opStart        = opLetter systemFDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= ["\\","->","="]
   , reservedNames  = ["let","in","ni","true","false","fix","if","then","else"]
   , caseSensitive  = False
   }

--Parsec lexer
lexer::TokenParser a
lexer = makeTokenParser systemFDef

parseProgram :: String -> Either ParseError Exp
parseProgram = parse (whiteSpace lexer *> parseExp <* eof) ""

parseExp :: Parser Exp
parseExp = try parseApp
       <|> parseExp'
       <?> "exp"

parseExp' :: Parser Exp
parseExp' = Var <$> parseVar
        <|> Con <$> parseConst
        <|> parseLambda
        <|> parseLet
        <|> parseFix
        <?> "exp'"

{-
parseOperator :: Parser Token
parseOperator =  choice (map parseOp gmlOperators)
             <?> "operator"
       where parseOp s = Operator s <$ reserved gmlLexer s
-}

parseVar :: Parser Var
parseVar = identifier lexer
        <?> "Variable"

parseConst :: Parser Con
parseConst = Bool <$> parseBoolean
         <|> Real <$> parseReal
         <?> "constant"
         
parseBoolean :: Parser Bool
parseBoolean =  True  <$ reserved lexer "true"
            <|> False <$ reserved lexer "false" 
            <?> "boolean"

--Customized number lexer, parsec's standard number lexer is not signed, so added that
parseReal :: Parser Double
parseReal = either fromInteger id <$> naturalOrFloat lexer
         <?> "real"

parseLambda :: Parser Exp
parseLambda = (\_ args _ exp -> foldr Lam exp args)
          <$> reservedOp lexer "\\"
          <*> many1 parseVar 
          <*> reservedOp lexer "->"
          <*> parseExp
          <?> "Lambda"
          
parseApp :: Parser Exp
parseApp = App <$> parseExp' <*> parseExp
        <?> "Function application"

parseFix :: Parser Exp
parseFix = Fix <$> (reserved lexer "fix" *> parseExp)
        <?> "FixPoint"

parseLet :: Parser Exp
parseLet = (\_ vs _ e _ -> Let vs e)
       <$> reserved lexer "let"
       <*> parseLet'
       <*> reserved lexer "in"
       <*> parseExp
       <*> reserved lexer "ni"
       <?> "Let"
    where parseLet' = foldr (\(var,exp) m -> Map.insert var exp m) Map.empty
                  <$> sepBy1 ((,) <$> parseVar <*> (reservedOp lexer "=" *> parseExp)) (semi lexer)


