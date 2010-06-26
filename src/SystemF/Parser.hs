module SystemF.Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Prim

import SystemF.Inference
import SystemF.Types
import SystemF.Units

import qualified Data.Map as Map
import Control.Applicative hiding ((<|>))

type Parser = Parsec String ()

keywords::[String]
keywords = ["let","in","ni","True","False","fix","if","then","else","fi"]

tyconsts::[String]
tyconsts = ["Bool","Real"]

unconsts::[(String,String)]
unconsts = [("m","L")
           ,("s","T")
           ,("kg","M")]

unparseconsts :: [String]
unparseconsts = map fst unconsts

--Definitions for the lexer created by parsec, see parsec documentation 2.8/2.9 and refernce guide
systemFDef::LanguageDef a
systemFDef
 = LanguageDef
   { commentStart   = "{-"
   , commentEnd     = "-}"
   , commentLine    = "--"
   , nestedComments = True
   , identStart     = letter
   , identLetter    = alphaNum <|> char '_'
   , opStart        = opLetter systemFDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= ["\\","->","=","::","^","-","."]
   , reservedNames  = keywords ++ tyconsts ++ unparseconsts
   , caseSensitive  = True
   }

--Parsec lexer
lexer::TokenParser a
lexer = makeTokenParser systemFDef

parseProgram :: String -> Either ParseError Exp
parseProgram = parse (whiteSpace lexer *> parseExp <* eof) ""

parseExp :: Parser Exp
parseExp = (\(e:es) -> foldl App e es) <$> many1 parseExp'
       <?> "exp"

parseExp' :: Parser Exp
parseExp' = Var <$> parseVar
        <|> Con <$> parseConst
        <|> parseLambda
        <|> parseLet
        <|> parseFix
        <|> parseIf
        <|> parens lexer parseExp
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
         <|> (flip Real) UnUnit <$> parseReal
         <?> "constant"
         
parseBoolean :: Parser Bool
parseBoolean =  True  <$ reserved lexer "True"
            <|> False <$ reserved lexer "False" 
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

parseIf :: Parser Exp
parseIf = (\_ c _ t _ e _ -> If c t e)
      <$> reserved lexer "if"
      <*> parseExp
      <*> reserved lexer "then"
      <*> parseExp
      <*> reserved lexer "else"
      <*> parseExp
      <*> reserved lexer "fi"

parseLet :: Parser Exp
parseLet = (\_ vs _ e _ -> foldr (\(ty,var,val) e -> Let var val e ty) e vs)
       <$> reserved lexer "let"
       <*> parseLets
       <*> reserved lexer "in"
       <*> parseExp
       <*> reserved lexer "ni"
       <?> "Let"
    where parseLets = sepBy1 parseLet' (semi lexer) 
          parseLet' = (,,)
                  <$> option Nothing (Just <$> parseType) 
                  <*> parseVar
                  <*> (reservedOp lexer "=" *> parseExp)
        
parseType :: Parser Ty
parseType = braces lexer parseTyFunc
            
parseType' :: Parser Ty
parseType' = (TyCon <$> parseTyConst)
         <|> (TyVar <$> parseVar)
         <|> parens lexer parseTyFunc

parseTyFunc :: Parser Ty
parseTyFunc = (\(v:vs) -> foldl (flip TyFun) v vs) . reverse
          <$> sepBy1 parseType' (reservedOp lexer "->")
          <?> "Type Function"
        
parseTyConst :: Parser TyCon
parseTyConst = (TyBool <$  reserved lexer "Bool")
           <|> (TyReal <$> (reserved lexer "Real"
                           *> option UnUnit parseUnit))

parseUnit :: Parser Un
parseUnit = (\(x:xs) -> foldl UnProd x xs)
        <$> squares lexer (many1 parseUnit')
        <?> "Units"
        
parseUnit' :: Parser Un
parseUnit' = createUnit
         <$> (UnVar <$> parseVar 
         <|> parseUnConst) 
         <*> (option False (True  <$ reservedOp lexer "-"
                        <|> False <$ reservedOp lexer "^"))
         <*> option 1 (decimal lexer)
         <?> "Unit"
         where  createUnit ty neg pow = if neg 
                                        then UnInv $ createUnitProd ty pow
                                        else createUnitProd ty pow
                createUnitProd ty 1   = ty
                createUnitProd ty n   = UnProd ty $ createUnitProd ty (n-1)
         
parseUnConst :: Parser Un
parseUnConst = choice $ map parseSingle unconsts
    where  parseSingle (un,dim) = UnCon dim <$ reserved lexer un
