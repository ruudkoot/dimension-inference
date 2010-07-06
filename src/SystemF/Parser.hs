module SystemF.Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String hiding (Parser)
import Text.ParserCombinators.Parsec.Expr

import SystemF.Inference
import SystemF.Types
import SystemF.Dimensions

import qualified Data.Map as Map
import Control.Applicative hiding ((<|>))
import Control.Monad

type Parser = Parsec String ()

keywords::[String]
keywords = ["let","in","ni","True","False","fix","if","then","else","fi"]

tyconsts::[String]
tyconsts = ["Bool","Real"]

unparseconsts :: [String]
unparseconsts = map snd dimensions

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
   , reservedOpNames= ["\\","->","=","::","^","-", "+", "."]
   , reservedNames  = keywords ++ tyconsts
   , caseSensitive  = True
   }
               
--Parsec lexer
lexer::TokenParser a
lexer = makeTokenParser systemFDef

parseProgram :: String -> Either ParseError Exp
parseProgram = parse (whiteSpace lexer *> parseExp <* eof) ""

     
parseExp :: Parser Exp
parseExp =  parseLambda
        <|> parseLet
        <|> parseFix
        <|> parseIf
        <|> parseExpression
        <?> "exp'"
        
parseExpression :: Parser Exp
parseExpression = buildExpressionParser opParsers parseFunc

parseFunc :: Parser Exp
parseFunc = (\(e:es) -> foldl App e es) <$> many1 parseExp'
          
parseExp' :: Parser Exp
parseExp' = Con <$> parseConst
        <|> Var <$> parseVar
        <|> parens lexer parseExp
        <?> "exp"

makeBinOp :: String -> GenParser Char () (Exp -> Exp -> Exp)
makeBinOp op = reservedOp lexer op >> return (\arg1 arg2 -> App (App (Var op) arg1) arg2)  
            
opParsers = map (map (\x -> Infix (makeBinOp $ fst x) AssocLeft)) operators

operators::[[(String, String)]]
operators = [[("*",   "{ Real [a] -> Real [b] -> Real [a b] }")
             ,("/",   "{ Real [a] -> Real [b] -> Real [a b-1] }")]
            ,[("+",   "{ Real [a] -> Real [a] -> Real [a] }")
             ,("-",   "{ Real [a] -> Real [a] -> Real [a] }")]
            ,[("and", "{ Bool -> Bool -> Bool }")
             ,("or",  "{ Bool -> Bool -> Bool }")]
            ,[("<",   "{ Real [a] -> Real [a] -> Real [a] }")
             ,(">",   "{ Real [a] -> Real [a] -> Real [a] }")
             ,("==",  "{ a -> a -> Bool }")]
            ]
            
            -- = Map.Map String TyScheme
operatorEnv :: TyEnv 
operatorEnv = foldr addOp Map.empty.concat $ operators
        where addOp (op,ty) ac = Map.insert op (parseT ty) ac
              parseT s = case parse (whiteSpace lexer *> parseType <* eof) "" s of
                           (Left e)   -> error $ "Parsing type:"++show s
                           (Right ty) -> generalize Map.empty ty
                           
parseVar :: Parser Var
parseVar = identifier lexer
        <?> "Variable"

parseConst :: Parser Con
parseConst = createReal <$> parseReal <*> option DimUnit parseDimension  
         <|> Bool <$> parseBoolean
         <?> "constant"
         where 
                createReal value dim = Real value dim

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
                           *> option DimUnit parseDimension))

parseDimension :: Parser Dim
parseDimension = (\(x:xs) -> foldl DimProd x xs)
        <$> squares lexer (many1 parseDimension')
        <?> "Dimensions"
        
parseDimension' :: Parser Dim
parseDimension' = createDimension
         <$> (parseDimConst
         <|> DimVar <$> parseVar) 
         <*> (option False (True  <$ reservedOp lexer "-"
                        <|> False <$ reservedOp lexer "^"))
         <*> option 1 (decimal lexer)
         <?> "Dimension"
         where  createDimension ty neg pow = if neg 
                                             then DimInv $ createDimensionProd ty pow
                                             else createDimensionProd ty pow
                createDimensionProd ty 1   = ty
                createDimensionProd ty n   = DimProd ty $ createDimensionProd ty (n-1)
         
parseDimConst :: Parser Dim
parseDimConst = choice $ map parseSingle dimensions
    where  parseSingle (dim, un) = DimCons dim <$ symbol lexer un
