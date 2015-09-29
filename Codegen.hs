module Codegen where

import Error
import Schemish
import qualified C
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data FnData = FnData
            { fnName :: Name
            , fnRet :: C.Type
            , fnArgs :: [(C.Type, Name)]
            , fnBody :: [C.Expr]
            }

data CodegenState = CodegenState
                  { numTemps :: Int
                  , typeEnv :: M.Map Name Type
                  }

type Codegen a = StateT CodegenState (Except Error) a

runCodegen :: Codegen a -> Either Error a
runCodegen x = runExcept $ evalStateT x defaultState
  where defaultState = CodegenState 0 defaultTypeEnv 

defaultTypeEnv :: M.Map Name Type
defaultTypeEnv = M.empty

--typecheck :: [Expr] -> Codegen ()
--typecheck = mapM_ typecheckExpr 

getType :: Expr -> Codegen Type
getType (Lit (LInt _))   = return Int
getType (Lit (LReal _))  = return Double
getType (Lit (LIdent s)) = do
    env <- gets typeEnv
    case M.lookup s env of
      Just t -> return t
      _      -> throwError $ NotFound s
getType (App f xs) = do
    ft <- getType f
    case ft of
      Func args ret -> return ret
      _             -> throwError $ WrongTypeToApply ft
getType (Def (Var name t) val) = do
    valtype <- getType val
    if t /= valtype
      then throwError $ CantMatchType valtype t
      else do env <- gets typeEnv
              modify $ \cg -> cg { typeEnv = M.insert name t env }
              return Void
getType (Defun (Var name ret) args body) = do
    xs <- mapM getType body
    if last xs /= ret
      then throwError $ CantMatchType (last xs) ret
      else do let func = Func (map varType args) ret
              env <- gets typeEnv
              modify $ \cg -> cg { typeEnv = M.insert name func env }
              return Void
  where varType (Var _ t) = t

getTemp :: Codegen Name
getTemp = do
    temps <- gets numTemps
    modify $ \cg -> cg { numTemps = temps + 1 }
    return $ "_temp" ++ show temps

denest :: [Expr] -> Codegen [Expr]
denest xs = do
    ys <- mapM denestExpr xs
    return $ concat $ map (\(a, b) -> a ++ [b]) ys

denestIf :: Expr -> Expr -> Expr -> Expr -> Codegen ([Expr], Expr)
denestIf var x y z = do
    cond  <- denestExpr x
    left  <- denestExpr (App (Lit (LIdent "set")) [var, y])
    right <- denestExpr (App (Lit (LIdent "set")) [var, z])
    let pre = [ App (Lit (LIdent "if")) [ snd cond, snd left, snd right ] ]
        res = var 
    return (fst cond ++ fst left ++ fst right ++ pre, res)

denestExpr :: Expr -> Codegen ([Expr], Expr)
denestExpr (App (Lit (LIdent "if")) [x, y, z]) = do
    temp    <- getTemp
    retType <- getType y
    (ifpre, _) <- denestIf (Lit (LIdent temp)) x y z
    return (Decl (Var temp retType) : ifpre, Lit (LIdent temp))

denestExpr (App (Lit (LIdent "set")) [var, App (Lit (LIdent "if")) [x, y, z]]) = do
    (ifpre, _) <- denestIf var x y z
    return (ifpre, var)

denestExpr (Def var@(Var name t) (App (Lit (LIdent "if")) [x, y, z])) = do
    (ifpre, _) <- denestIf (Lit (LIdent name)) x y z
    return (Decl var : ifpre, Lit (LIdent name))

denestExpr (App (Lit (LIdent "begin")) body) = do
    xs <- mapM denestExpr body
    return (init $ concat (map (\(ys,y) -> ys ++ [y]) xs) , snd (last xs))

denestExpr (App f xs) = do
   (a, fn) <- denestExpr f
   xs      <- mapM denestExpr xs
   let pre = a ++ concat (map fst xs)
       res = App fn (map snd xs)
   return (pre, res)

denestExpr exp = return ([], exp)

gen :: Expr -> Codegen C.Expr
gen (Lit (LInt n)) = return $ C.Lit (C.LInt n)
gen (Lit (LReal n)) = return $ C.Lit (C.LReal n)
gen (Lit (LIdent n)) = return $ C.Lit (C.LIdent n)

gen (App (Lit (LIdent "set")) [(Lit (LIdent x)), y]) = do
    val <- gen y
    return $ C.Assign x val

