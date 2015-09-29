module Error where

import Schemish
import Text.ParserCombinators.Parsec (ParseError)

data Error = CantMatchType Type Type
           | WrongTypeToApply Type
           | NotFound Name
           deriving Eq

instance Show Error where
    show (NotFound name) = "symbol " ++ name ++ " does not exist in this context"
    show (WrongTypeToApply t) = "wrong type to apply: " ++ show t
    show (CantMatchType x y) = "could not match types " ++ show x ++ " and " ++ show y

