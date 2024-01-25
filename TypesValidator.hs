module TypesValidator where

import AbsMiLatte
import Control.Monad.State
import Data.Map (Map,lookup,insert,empty,filter)
import ErrorsMiLatte
import TypesMiLatte

runValidator :: Program -> IO ()
runValidator prog = do
  liftIO (evalStateT (validate prog) Data.Map.empty)

validate :: Program -> ValidStateT ()
validate (Program p) = do
    readFuns p
    validateProgram p

readFuns :: [TopDef] -> ValidStateT ()
readFuns [] = return ()
readFuns ((FnDef ftype ident args block):xs) = do 
    putIntoEnv ident ftype (FunV (args,block))
    readFuns xs

validateProgram :: [TopDef] -> ValidStateT ()
validateProgram [] = return ()
validateProgram (x:xs) = do
    validateTopDef x
    validateProgram xs

validateTopDef :: TopDef -> ValidStateT ()
validateTopDef (FnDef ftype ident args (Block block)) = do
    e@(etype,elemtype) <- getVEnvElem ident
    case elemtype of
        FunV f -> do 
            venv <- get
            prepareFunEnv args venv 
            validateTopDefStmt block e ident
            put venv
        _ -> vthrowErrorNoObj $ show ident ++ "is not a function"

prepareFunEnv :: [Arg] -> VEnv -> ValidStateT ()
prepareFunEnv args oldEnv = do
    getFunctionsIntoEnvV
    initArgsV args oldEnv

initArgsV :: [Arg] -> VEnv -> ValidStateT ()
initArgsV [] _ = return ()
initArgsV ((Arg atype i):xs) oldEnv = do
    case atype of
        Func types block -> putIntoEnv i atype LambdaV
        _ -> putIntoEnv i atype VarV

getFunctionsIntoEnvV :: ValidStateT ()
getFunctionsIntoEnvV = do
    venv <- get
    put $ (Data.Map.filter (\n -> isFunV n) venv)

isFunV :: VEnvElem -> Bool
isFunV (_,(FunV _)) = True
isFunV _ = False

validateTopDefStmt :: [StmtOrFn] -> VEnvElem -> Ident -> ValidStateT ()  
validateTopDefStmt ((StmtF (FnDef ftype ident args block)):xs) ve i = do
    venv <- get
    putIntoEnv ident ftype (FunV (args,block))
    validateTopDefStmt xs ve i
    put venv --restoring the original env
validateTopDefStmt ((StmtS stmt):xs) ve@(ftype,(FunV (args,block))) ident = do
  case stmt of
    Ret expr -> do
        if ftype == Void then vthrowError "Void function cannot return expression" ident
        else do
            etype <- getExpressionType expr
            areTypesEqual etype ftype ("Function of type " ++ show ftype ++ " cannot return expression of type " ++ show etype)
            return ()
    VRet -> do
        if ftype /= Void then vthrowError "Non-void function cannot return void" ident
        else do
            return ()
    Cond expr stmt -> do
        checkBool expr
        validateTopDefStmt ((StmtS stmt):xs) ve ident
    CondElse expr stmt1 stmt2 -> do
        checkBool expr   
        validateTopDefStmt ((StmtS stmt1):(StmtS stmt2):xs) ve ident
    While expr stmt -> do
        checkBool expr
        validateTopDefStmt ((StmtS stmt):xs) ve ident
    BStmt (Block block) -> do
        validateTopDefStmt (block ++ xs) ve ident
    s -> do 
        checkStatement s    
        validateTopDefStmt xs ve ident

validateFunArgs :: [Arg] -> [Expr] -> Ident -> ValidStateT ()
validateFunArgs [] [] i = return ()
validateFunArgs as@(x:xs) es@(y:ys) i = do
    if length as /= length es then vthrowErrorNoObj $ "Wrong number of arguments for function application " ++ show i
    else do
        validateArg x y i
        validateFunArgs xs ys i

validateArg :: Arg -> Expr -> Ident -> ValidStateT ()
validateArg (Arg atype ident) expr fid= do
    etype <- getExpressionType expr
    areTypesEqual atype etype ("Argument " ++ show ident ++ " for function " ++ show fid ++ " is of wrong type " ++ show etype)

getLambdaArgsTypes :: [Arg] -> [Type]
getLambdaArgsTypes [] = []
getLambdaArgsTypes ((Arg atype ident):as) = [atype] ++ (getLambdaArgsTypes as)

getExpressionType :: Expr -> ValidStateT Type
getExpressionType (Lambda ltype args block) = do
    return $ Func ltype (getLambdaArgsTypes args)
getExpressionType (EVar ident) = getVarType ident
getExpressionType (ELitInt i) = return Int 
getExpressionType (ELitTrue) = return Bool
getExpressionType (ELitFalse) = return Bool
getExpressionType (EApp (Ident "print") exprs) = do
    return Str
getExpressionType (EApp ident exprs) = do
    (ftype,etype) <- getVEnvElem ident
    case etype of 
        LambdaV -> do 
            return ftype
        FunV (args,block) -> do
            validateFunArgs args exprs ident
            validateTopDef (FnDef ftype ident args block)
            return ftype
getExpressionType (EString string) = return Str
getExpressionType (EArr ident arrbrs) = do
    checkArrIndeces arrbrs
    getVarType ident
getExpressionType (Neg expr) = do
    etype <- getExpressionType expr
    case etype of
        Int -> return Int
        _ -> vthrowErrorNoObj $ "Expression should be int but is of type " ++ (show etype)
getExpressionType (Not expr) = do
    etype <- getExpressionType expr
    case etype of
        Bool -> return Bool
        _ -> vthrowErrorNoObj $ "Expression should be boolean but is of type " ++ (show etype)
getExpressionType (EMul expr1 mulop expr2) = checkNumericals expr1 expr2
getExpressionType (EAdd expr1 addop expr2) = checkNumericals expr1 expr2
getExpressionType (ERel expr1 relop expr2) = do
    checkNumericals expr1 expr2
    return Bool
getExpressionType (EAnd expr1 expr2) = checkBooleans expr1 expr2
getExpressionType (EOr expr1 expr2) = checkBooleans expr1 expr2

checkArrIndeces :: [ArrBr] -> ValidStateT ()
checkArrIndeces [] = return ()
checkArrIndeces ((ArrBrac x):xs) = do
    checkInt x
    checkArrIndeces xs

checkInt :: Expr -> ValidStateT ()
checkInt expr = do
    etype <- getExpressionType expr
    case etype of
        Int -> return ()
        _ -> vthrowErrorNoObj $ "Expression " ++ show expr ++ " should be int but is of type " ++ (show etype)

checkBool :: Expr -> ValidStateT ()
checkBool expr = do
    etype <- getExpressionType expr
    case etype of
        Bool -> return ()
        _ -> vthrowErrorNoObj $ "Expression " ++ show expr ++ " should be boolean but is of type " ++ (show etype)

checkNumericals :: Expr -> Expr -> ValidStateT Type
checkNumericals expr1 expr2 = do
    checkInt expr1
    checkInt expr2
    return Int

checkBooleans :: Expr -> Expr -> ValidStateT Type
checkBooleans expr1 expr2 = do
    checkBool expr1
    checkBool expr2
    return Bool

getVarType :: Ident -> ValidStateT Type
getVarType i = do
    (t,_) <- getVEnvElem i
    return t

getElemType :: Ident -> ValidStateT ElemType
getElemType i = do
    (_,t) <- getVEnvElem i
    return t

getVEnvElem :: Ident -> ValidStateT VEnvElem
getVEnvElem i = do
    venv <- get
    case Data.Map.lookup i venv of
        Just e -> return e
        Nothing -> vthrowError "Variable not declared, couldn't infer type" i    

checkStatement :: Stmt -> ValidStateT ()
checkStatement Empty = return ()
checkStatement (Decl dtype []) = return ()
checkStatement (Decl dtype (x:xs)) = do
    case x of
        NoInit ident -> do
            putIntoEnv ident dtype VarV
            checkStatement (Decl dtype xs)
        Init ident expr -> do
            etype <- getExpressionType expr
            areTypesEqual dtype etype ("Cannot declare variable " ++ show ident ++ " with type " ++ show dtype ++ " with expression of type " ++ show etype)
            putIntoEnv ident dtype VarV
            checkStatement (Decl dtype xs)
checkStatement (ArrDecl atype ident arrbrs) = do 
    checkArrIndeces arrbrs
    putIntoEnv ident atype ArrV
checkStatement (Ass ident expr) = do
    vtype <- getVarType ident
    etype <- getExpressionType expr
    areTypesEqual vtype etype ("Types not match in expression " ++ show ident ++ " = " ++ show expr)
checkStatement (AssArr ident arrbrs expr) = do
    elemType <- getElemType ident
    case elemType of
        ArrV -> do
            checkArrIndeces arrbrs
            vtype <- getVarType ident
            etype <- getExpressionType expr
            areTypesEqual vtype etype ("Types not match in expression " ++ show ident ++ " = " ++ show expr)
        _ -> vthrowError "Declared object is not an array" ident   
checkStatement (Incr ident) = do
    vtype <- getVarType ident
    case vtype of
        Int -> return ()
        _ -> vthrowErrorNoObj $ "Cannot increment variable " ++ show ident ++ " of type " ++ show vtype
checkStatement (Decr ident) = do
    vtype <- getVarType ident
    case vtype of
        Int -> return ()
        _ -> vthrowErrorNoObj $ "Cannot decrement variable " ++ show ident ++ " of type " ++ show vtype
checkStatement (SExp expr) = do 
    getExpressionType expr
    return ()
checkStatement (Break) = return ()
checkStatement (Continue) = return ()

areTypesEqual :: Type -> Type -> String -> ValidStateT ()
areTypesEqual (Func t1 ts) (Func t2 ts2) errmsg = do
    if t1 /= t2 then vthrowErrorNoObj errmsg
    else return ()
areTypesEqual (Func t1 ts) t2 errmsg = do
    if t1 /= t2 then vthrowErrorNoObj errmsg
    else return ()
areTypesEqual t1 (Func t2 ts) errmsg = do
    if t1 /= t2 then vthrowErrorNoObj errmsg
    else return ()
areTypesEqual t1 t2 errmsg = do
    if t1 /= t2 then vthrowErrorNoObj errmsg
    else return ()

isInEnv :: Ident -> ValidStateT Bool
isInEnv i = do
    venv <- get
    case Data.Map.lookup i venv of
        Just x -> return True
        Nothing -> return False

putIntoEnv :: Ident -> Type -> ElemType -> ValidStateT ()
putIntoEnv i t vt = do
    alreadDeclared <- isInEnv i
    if alreadDeclared then vthrowErrorNoObj $ "An element already in scope: " ++ show i
    else do
        venv <- get
        put (Data.Map.insert i (t,vt) venv)
        