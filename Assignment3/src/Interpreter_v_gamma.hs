{-# LANGUAGE RecordWildCards, FlexibleInstances, FlexibleContexts, PatternSynonyms #-}

module Interpreter ( exec, Value(..), IIO(..), Interpreter(..), emptyEnv ) where

import AbsCPP
import ErrM
import PrintCPP

import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad.State ( StateT, get, modify, foldM )

data Value = VInt Integer
           | VDouble Double
           | VVoid
           | VUndefined deriving Eq

pattern VTrue = VInt 1
pattern VFalse = VInt 0

class Monad i => Interpreter i where
    printInt :: Integer -> i ()
    printDouble :: Double -> i ()
    readInt :: i Integer
    readDouble :: i Double

instance Interpreter IO where
    printInt i = putStrLn $ show i
    printDouble d = putStrLn $ show d
    readInt = do
        line <- getLine
        return (read line)
    readDouble = do
        line <- getLine
        return (read line)

data IIO = IIO {
    inputs :: [Value]
  , outputs :: [Value]
}

instance Interpreter (StateT IIO Err) where
    printInt i = modify (\io@IIO{..} -> io{outputs = VInt i:outputs}) 
    printDouble d = modify (\io@IIO{..} -> io{outputs = VDouble d:outputs}) 
    readInt = do
        IIO{..} <- get
        case inputs of
            (VInt i:_) -> do
                modify (\io@IIO{..} -> io{inputs = tail inputs}) 
                return i
            _ -> fail $ "Invalid input given. expected an int."
    readDouble = do
        IIO{..} <- get
        case inputs of
            (VDouble d:_) -> do
                modify (\io@IIO{..} -> io{inputs = tail inputs}) 
                return d
            (VInt i:_) -> do
                modify (\io@IIO{..} -> io{inputs = tail inputs}) 
                return $ fromInteger i
            _ -> fail $ "Invalid input given. expected a double."

type Sig = Map Id Def
type Context = Map Id Value
type Env = (Sig, [Context])

emptyEnv :: Env
emptyEnv = (M.empty, [M.empty]) 

extendSig :: Interpreter i => Env -> Def -> i Env
extendSig (sig,ctxt) def@(DFun _ i _ _) = return $ (M.insert i def sig, ctxt)

lookupSig :: Interpreter i => Id -> Env -> i Def
lookupSig i (sig,_) = case M.lookup i sig of
    (Just f) -> return f
    Nothing -> fail $ "Error, could not find " ++ printTree i ++ "."

extendContext :: Interpreter i => Env -> Id -> Value -> i Env
extendContext (sig, []) i v = return $ (sig, [M.insert i v M.empty]) -- should possibly be an internal error
extendContext (sig, c:ctxt) i v = return $ (sig, M.insert i v c:ctxt)

updateContext :: Interpreter i => Env -> Id -> Value -> i Env
updateContext (sig, []) i v = fail $ "Internal error, " ++ printTree i ++ " could not be found."
updateContext (sig, c:ctxt) i v = case M.lookup i c of
    (Just _) -> return (sig, M.insert i v c:ctxt)
    Nothing -> do
        (_,ctxt') <- updateContext (sig, ctxt) i v
        return $ (sig, c:ctxt')

lookupContext :: Interpreter i => Id -> Env -> i Value
lookupContext i (_,[]) = fail $ "Error, could not find " ++ printTree i ++ "."
lookupContext i (sig,c:ctxt) = case M.lookup i c of
    (Just f) -> return f
    Nothing -> lookupContext i (sig,ctxt)

push :: Interpreter i => Env -> i Env
push (sig, ctxts) = return (sig, M.empty:ctxts)


pop :: Interpreter i => Env -> i Env
pop (sig, []) = fail $ "Internal error, can't pop an enpty context."
pop (sig, c:ctxt) = return (sig, ctxt)

pushPop :: Interpreter i => Env -> (Env -> i (a, Env)) -> i (a, Env)
pushPop env f = do
    env' <- push env
    (val,env'') <- f env'
    env''' <- pop env''
    return (val,env''')

exec :: Interpreter i => Program -> i ()
exec (PDefs defs) = do
    env@(sigs,_) <- foldM extendSig emptyEnv defs
    (DFun _ _ _ stms) <- lookupSig (Id "main") env
    evalStms env stms
    return ()

evalStms :: Interpreter i => Env -> [Stm] -> i (Maybe Value, Env)
evalStms env [] = return (Nothing, env)
evalStms env (s:tms) = do
    (v, env') <- evalStm env s
    if v == Nothing then evalStms env' tms
    else return (v, env')


evalStm :: Interpreter i => Env -> Stm -> i (Maybe Value, Env)
{-
evalStm env (SExp e) = 
evalStm env (SDecls _ ids) = 
evalStm env (SInit _ i e) = 
evalStm env SReturnVoid = 
-}
evalStm env (SReturn e) = do
    (v, env') <- evalExp env e
    return (Just v, env')
{-
evalStm env (SWhile e stm) = 
evalStm env (SBlock stms) = 
evalStm env (SIfElse e stm1 stm2) = 
-}
evalStm _ stm = 
    fail $ "Missing case in evalStm " ++ printTree stm ++ "\n"

evalExp :: Interpreter i => Env -> Exp -> i (Value, Env)
{-
evalExp env ETrue = 
evalExp env EFalse = 
-}
evalExp env (EInt i) = return (VInt i, env)
{-
evalExp env (EDouble d) = 
evalExp env (EString _) = 
evalExp env (EId i) = 
evalExp env (EApp i exps) = 
evalExp env (EPIncr e@(EId i)) = 
evalExp env (EPIncr e) = 
evalExp env (EPDecr e@(EId i)) = 
evalExp env (EPDecr e) = 
evalExp env (EIncr e@(EId i)) = 
evalExp env (EIncr e) = 
evalExp env (EDecr e@(EId i)) = 
evalExp env (EDecr e) = 
evalExp env (ETimes e1 e2) = 
evalExp env (EDiv e1 e2)   = 
evalExp env (EPlus e1 e2)  = 
evalExp env (EMinus e1 e2) = 
evalExp env (ELt e1 e2)    = 
evalExp env (EGt e1 e2)    = 
evalExp env (ELtEq e1 e2)  = 
evalExp env (EGtEq e1 e2)  = 
evalExp env (EEq e1 e2)    =
evalExp env (ENEq e1 e2) =
evalExp env (EAnd e1 e2) = 
evalExp env (EOr e1 e2) = 
evalExp env (EAss (EId i) e) = 
evalExp env (EAss _ _) = 
evalExp env (ETyped e _) = 
-}
evalExp _ e = fail $ "Missing case in evalExp." ++ printTree e ++ "\n"

evalExpList :: Interpreter i => Env -> [Exp] -> i ([Value], Env)
evalExpList env [] = return ([], env)
evalExpList env (e:exps) = do
    (v, env') <- evalExp env e
    (vs, env'') <-evalExpList env' exps
    return (v:vs, env'')


applyFun :: Interpreter i => (Value -> Value -> i Value) -> Env -> Exp -> Exp -> i (Value, Env)
applyFun f env e1 e2 = do
    (vs, env') <- evalExpList env [e1,e2]
    case vs of
        [v1,v2] -> do
            v <- f v1 v2
            return (v, env')


addValue :: Interpreter i => Value -> Value -> i Value
addValue (VInt    u) (VInt    v) = return $ VInt $ u + v
addValue (VDouble u) (VDouble v) = return $ VDouble $ u + v
addValue (VDouble u) (VInt    v) = return $ VDouble $ u + (fromInteger v)
addValue (VInt    u) (VDouble v) = return $ VDouble $ (fromInteger u) + v
addValue _ _ = fail $ "Internal error, trying to add incompatible types."


subValue :: Interpreter i => Value -> Value -> i Value
subValue (VInt    u) (VInt    v) = return $ VInt $ u - v
subValue (VDouble u) (VDouble v) = return $ VDouble $ u - v
subValue (VDouble u) (VInt    v) = return $ VDouble $ u - (fromInteger v)
subValue (VInt    u) (VDouble v) = return $ VDouble $ (fromInteger u) - v
subValue _ _ = fail $ "Internal error, trying to sub incompatible types."


mulValue :: Interpreter i => Value -> Value -> i Value
mulValue (VInt    u) (VInt    v) = return $ VInt $ u * v
mulValue (VDouble u) (VDouble v) = return $ VDouble $ u * v
mulValue (VDouble u) (VInt    v) = return $ VDouble $ u * (fromInteger v)
mulValue (VInt    u) (VDouble v) = return $ VDouble $ (fromInteger u) * v
mulValue _ _ = fail $ "Internal error, trying to mul incompatible types."


divValue :: Interpreter i => Value -> Value -> i Value
divValue (VInt    u) (VInt    v) | v /= 0    = return $ VInt $ u `div` v
                                 | otherwise = fail $ "Error division by 0."
divValue (VDouble u) (VDouble v) | v /= 0    = return $ VDouble $ u / v
                                 | otherwise = fail $ "Error division by 0."
divValue (VDouble u) (VInt    v) = divValue (VDouble u) (VDouble $ fromInteger v)
divValue (VInt    u) (VDouble v) = divValue (VDouble $ fromInteger u) (VDouble v)
divValue _ _ = fail $ "Internal error, trying to mul incompatible types."


ltValue :: Interpreter i => Value -> Value -> i Value
ltValue (VInt    u) (VInt    v) | u < v     = return $ VTrue
                                | otherwise = return $ VFalse
ltValue (VDouble u) (VDouble v) | u < v     = return $ VTrue
                                | otherwise = return $ VFalse
ltValue (VDouble u) (VInt    v) = ltValue (VDouble u) (VDouble $ fromInteger v)
ltValue (VInt    u) (VDouble v) = ltValue (VDouble $ fromInteger u) (VDouble v)
ltValue _ _ = fail $ "Internal error, trying to apply ltValue to incompatible types."


gtValue :: Interpreter i => Value -> Value -> i Value
gtValue (VInt    u) (VInt    v) | u > v     = return $ VTrue
                                | otherwise = return $ VFalse
gtValue (VDouble u) (VDouble v) | u > v     = return $ VTrue
                                | otherwise = return $ VFalse
gtValue (VDouble u) (VInt    v) = gtValue (VDouble u) (VDouble $ fromInteger v)
gtValue (VInt    u) (VDouble v) = gtValue (VDouble $ fromInteger u) (VDouble v)
gtValue _ _ = fail $ "Internal error, trying to apply gtValue to incompatible types."


negValue :: Interpreter i => Value -> i Value
negValue VFalse = return $ VTrue
negValue VTrue  = return $ VFalse
negValue _ = fail $ "Internal error, trying to apply negValue to incompatible types."