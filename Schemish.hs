module Schemish where

type Name = String

data Expr = App Expr [Expr]
          | Def Var Expr
          | Defun Var [Var] [Expr]
          | Decl Var
          | Lit Lit
          deriving Eq

instance Show Expr where
    show (App f xs) = "(" ++ show f ++ " " ++ unwords (map show xs) ++ ")"
    show (Def var val) = "(" ++ show var ++ " " ++ show val ++ ")"
    show (Defun f args body) =
      "(" ++ show f ++ " (" ++ unwords (map show args) ++ ") " ++ unwords (map show body) ++ ")"
    show (Decl var) = "(" ++ show var ++ ")"
    show (Lit l) = show l

data Var = Var Name Type
         deriving Eq

instance Show Var where
    show (Var name t) = name ++ ":" ++ show t

data Lit = LInt Int
         | LReal Double
         | LIdent Name
         deriving Eq

instance Show Lit where
    show (LInt i) = show i
    show (LReal x) = show x
    show (LIdent n) = n

data Type = Char
          | Int
          | UInt
          | Float
          | Double
          | Ptr Type
          | Func [Type] Type
          | Void
          deriving Eq

instance Show Type where
    show Char = "char"
    show Int = "int"
    show UInt = "uint"
    show Float = "float"
    show Double = "double"
    show (Ptr t) = "*" ++ show t
    show (Func args ret) =
      "[" ++ unwords (map show args) ++ " -> " ++ show ret ++ "]"

