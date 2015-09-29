module Parse where

import Schemish
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int, floating)

parseFile :: FilePath -> String -> Either ParseError [Expr]
parseFile = parse parseExprs

parseExprs :: Parser [Expr]
parseExprs = spaces *> many (parseExpr <* spaces)

parseExpr :: Parser Expr
parseExpr = parseUnaryNegate
        <|> parseAddress
        <|> parseDereference
        <|> parseDefun
        <|> parseDecl
        <|> parseDef
        <|> parseApp
        <|> Lit <$> parseLit
        <?> "expression"

parseUnaryNegate :: Parser Expr
parseUnaryNegate = do
    char '-'
    exp <- parseExpr
    return $ App (Lit (LIdent "-")) [exp]

parseAddress :: Parser Expr
parseAddress = do
    char '&'
    exp <- parseExpr
    return $ App (Lit (LIdent "&")) [exp]

parseDereference :: Parser Expr
parseDereference = do
    char '@'
    exp <- parseExpr
    return $ App (Lit (LIdent "@")) [exp]

parseDefun :: Parser Expr
parseDefun = do
    (ret, args) <- try $ do
      char '(' <* spaces
      ret <- parseVar <* spaces
      char '(' <* spaces
      args <- parseVar `sepEndBy` space
      return (ret, args)
    char ')' <* spaces
    body <- parseExprs <* spaces
    char ')'
    return $ Defun ret args body

parseDecl :: Parser Expr
parseDecl = do
    try $ do
      char '(' <* spaces
      var <- parseVar
      char ')'
      return $ Decl var

parseDef :: Parser Expr
parseDef = do
    var <- try $ do
      char '(' <* spaces
      parseVar
    spaces
    val <- parseExpr
    char ')'
    return $ Def var val

parseVar :: Parser Var
parseVar = do
    name <- parseName
    char ':'
    vartype <- parseType
    return $ Var name vartype

parseName :: Parser Name
parseName = do
    let illegal = ":'()& \t\n\r\f@" -- crying bearded monkey face
        digits  = ['0'..'9']
    first <- noneOf (illegal ++ digits)
    rest  <- many (noneOf illegal)
    return $ first : rest

parseType :: Parser Type
parseType = Ptr <$> (char '*' *> parseType)
        <|> Int <$ string "int"
        <|> UInt <$ string "uint"
        <|> Char <$ string "char"
        <|> parseFuncType
        <?> "type"

parseFuncType :: Parser Type
parseFuncType = do
    char '[' <* spaces
    args <- parseType `sepEndBy` space
    string "->" <* spaces
    ret  <- parseType <* spaces
    char ']'
    return $ Func args ret

parseApp :: Parser Expr
parseApp = do
    char '(' <* spaces
    xs <- parseExprs
    char ')'
    return $ App (head xs) (tail xs)

parseLit :: Parser Lit
parseLit = LIdent <$> parseName
       <|> try (LReal <$> floating)
       <|> LInt <$> int
       <?> "literal"

