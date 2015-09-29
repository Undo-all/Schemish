{-# LANGUAGE FlexibleInstances #-}

module C where

import Data.List (intercalate)

type Name = String

indent :: Int -> String
indent n = replicate (4*n) ' '

list :: (Pretty a) => [a] -> String
list xs = "(" ++ intercalate ", " (map pp xs) ++ ")"

block :: (Pretty a) => Int -> [a] -> String
block i xs = " {\n" ++ intercalate ";\n" (map (ppi (i+1)) xs) ++ ";\n" ++ indent i ++ "}"

class Pretty a where
    ppi :: Int -> a -> String
    pp :: a -> String
    pp = ppi 0

type File = [TopLevel]

instance Pretty File where
    ppi i f = intercalate "\n" $ map (ppi i) f

data TopLevel = Func Type Name [(Type, Name)] [Expr]
              | Global (Type, Name) (Maybe Lit)
              deriving (Eq, Show)

instance Pretty TopLevel where
    ppi i (Func (FuncPtr ret xs) name args body) =
      let typedefName = makeFnPtrName (ret : xs)
      in "typedef " ++ pp ret ++ typedefName ++ list xs ++
      "; " ++ typedefName ++ " " ++ name ++ list args ++
      block i body
      where makeFnPtrName = concat . map short
            short t = case t of
                        Char         -> "c"
                        Int          -> "i"
                        UnsignedInt  -> "ui"
                        Float        -> "f"
                        Double       -> "d"
                        Ptr t        -> "p" ++ short t
                        FuncPtr r xs -> "fpIr" ++ short ret ++ "a" ++
                                              concat (map short xs) ++ "I"
    ppi i (Global var val) = ppi i var ++ maybe "" ((++" = ") . pp) val ++ ";"

instance Pretty (Type, Name) where
    ppi i (FuncPtr ret args, n) = 
        ppi i ret ++ " (*" ++ n ++ ")" ++ list args
    ppi i (t, n) = ppi i t ++ " " ++ n

data Type = Char
          | Int
          | UnsignedInt
          | Float
          | Double
          | Ptr Type
          | FuncPtr Type [Type]
          deriving (Eq, Show)

instance Pretty Type where
    ppi i t = indent i ++ typeName t
      where typeName t =
              case t of
                Char        -> "char"
                Int         -> "int"
                UnsignedInt -> "unsigned int"
                Float       -> "float"
                Double      -> "double"
                Ptr t       -> typeName t ++ "*"
                FuncPtr _ _ -> error "unable to pretty-print lone function ptr"

data Expr = Local Type Name (Maybe Expr)
          | Assign Name Expr
          | If Expr [Expr] [Expr]
          | Lit Lit
          deriving (Eq, Show)

instance Pretty Expr where
    ppi i (Local t n val) =
      ppi i (t, n) ++ maybe "" ((++" = ") . pp) val
    ppi i (Assign name val) = indent i ++ name ++ " = " ++ pp val

data Lit = LInt Int
         | LReal Double
         | LIdent Name
         deriving (Eq, Show)

instance Pretty Lit where
    ppi i (LInt n) = indent i ++ show n
    ppi i (LReal x) = indent i ++ show x
    ppi i (LIdent n) = indent i ++ n

