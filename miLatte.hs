module Main where

import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import Data.Map (Map, insert, member, lookup, empty, filter, update)
import Control.Monad.State

import LexMiLatte
import ParMiLatte
import SkelMiLatte
import PrintMiLatte
import AbsMiLatte
import TypesMiLatte
import ErrorsMiLatte
import TypesValidator
import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn "Tokens:"
                          putStrLn $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do runValidator tree
                          runProgram tree
                          exitSuccess

showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run pProgram
    fs -> mapM_ (runFile pProgram) fs

runProgram :: Program -> IO ()
runProgram prog = do
  liftIO (evalStateT (executeProgram prog) emptyProgState)

emptyProgState = ([],Data.Map.empty,Data.Map.empty,0)

executeProgram :: Program -> ProgStateT ()
executeProgram (Program prog) = do
  readProgram prog
  executeMain
  return ()

readProgram :: [TopDef] -> ProgStateT ()
readProgram [] = return ()
readProgram (x:xs) = do
  readTopDef x
  readProgram xs

readTopDef :: TopDef -> ProgStateT ()
readTopDef (FnDef ftype ident args block) = do
  declareFunOrArr ident (FunEE (ftype,args,block))

executeMain :: ProgStateT ()
executeMain = do
  callFun (Ident "main") []
  return ()

callFun :: Ident -> [Expr] -> ProgStateT (Maybe Expr)
callFun i exprs = do
  case i of 
    (Ident "print") -> do 
      executePrint exprs
      return Nothing
    _  -> do 
      fun <- getEnvElem i
      case fun of
        FunEE (ftype,args,block) -> do
          (stackOld,envOld,_,_) <- get
          prepareProgState args exprs envOld
          ret <- executeFun fun
          (_,_,storeNew,siNew) <- get
          put (stackOld,envOld,storeNew,siNew)
          return ret 
        LambdaEE (ltype,types,args,lblock) ->
          case lblock of
            NonInit -> throwError "Lambda not initialized" (fun)
            LambdaBlock block -> do
              (stackOld,envOld,_,_) <- get
              prepareProgState args exprs envOld
              ret <- executeFun $ FunEE (ltype,args,block)
              (_,_,storeNew,siNew) <- get
              put (stackOld,envOld,storeNew,siNew)
              return ret 
        _ -> throwError "Not a function" (fun)

prepareProgState :: [Arg] -> [Expr] -> Env -> ProgStateT ()
prepareProgState args exprs oldEnv = do
  getFunctionsIntoEnv
  initArgs args exprs oldEnv
  
getFunctionsIntoEnv :: ProgStateT ()
getFunctionsIntoEnv = do
  (stack,env,store,si) <- get
  let newEnv = (Data.Map.filter (\n -> isFun n) env) in put (stack,newEnv,store,si)

isFun :: EnvElem -> Bool
isFun (FunEE _) = True
isFun _ = False

initArgs :: [Arg] -> [Expr] -> Env -> ProgStateT ()
initArgs [] [] _ = return ()
initArgs as@(a:args) es@(e:exprs) env = do
  if (isNumberOfArgsOk as es) then do
    declareArg a e env
    initArgs args exprs env
  else throwErrorNoObj "Wrong number of arguments"

declareArg :: Arg -> Expr -> Env -> ProgStateT ()
declareArg (Arg atype ident) expr env = do
  case expr of
    EVar i -> do
      case atype of
        Func _ _ -> do --Lambda
          l <- getLambdaEE i env
          declareFunOrArr ident l
        _ -> do --Variable
          loc <- getLoc i env
          (stack,env,store,si) <- get
          put (stack,Data.Map.insert ident (VarEE loc) env,store,si)
    _ -> executeDeclaration atype ((Init ident expr):[])

getLambdaEE :: Ident -> Env -> ProgStateT EnvElem
getLambdaEE i env = do
  case Data.Map.lookup i env of
    Just l -> return l
    _ -> throwError "Fun variable not initialized" i

getLoc :: Ident -> Env -> ProgStateT Loc
getLoc i env = do
  case Data.Map.lookup i env of
    Just (VarEE l) -> return l
    _ -> throwError "Not a variable" i

isNumberOfArgsOk :: [Arg] -> [Expr] -> Bool
isNumberOfArgsOk args exprs = (length args) == (length exprs)

executePrint :: [Expr] -> ProgStateT ()
executePrint (x:[]) = do
  s <- convertToString x
  printStr s
executePrint xs = do 
  throwError "Wrong number of arguments for print function" (length xs)

convertToString :: Expr -> ProgStateT String
convertToString (EString s) = return s
convertToString (EVar i) = do
  val <- getVarExpr i
  convertToString val
convertToString (ELitInt i) = return $ show i
convertToString ELitTrue = return "true"
convertToString ELitFalse = return "false"
convertToString (EArr i arrbrs) = do
  envElem <- getEnvElem i
  case envElem of
    ArrEE a -> do
      indeces <- getArrSizes arrbrs
      expr <- getArrayElem a indeces
      convertToString expr
    _ -> throwError "Not an array" envElem
convertToString o = do
  res <- evaluateExpression o
  convertToString res
  

printStr :: String -> ProgStateT ()
printStr s = do
  liftIO (putStrLn s)
  return ()

executeFun :: EnvElem -> ProgStateT (Maybe Expr)
executeFun (FunEE (Void,args,(Block block))) = do
  executeVoidFun block
  return Nothing
executeFun (FunEE (t,args,(Block block))) = do
  ret <- executeRetFun block
  return (Just ret)

executeRetFun :: [StmtOrFn] -> ProgStateT Expr
executeRetFun [] = throwErrorNoObj "Missing return statement"
executeRetFun ((StmtF x):xs) = do
  readTopDef x
  executeRetFun xs
executeRetFun ((StmtS x):xs) = do
  case x of
    Ret e -> evaluateExpression e
    VRet -> throwErrorNoObj "Missing expression in return statement"
    _ -> do 
      executeStatement x
      executeRetFun xs

executeVoidFun :: [StmtOrFn] -> ProgStateT ()
executeVoidFun [] = return ()
executeVoidFun ((StmtF x):xs) = do
  readTopDef x
  executeVoidFun xs
executeVoidFun ((StmtS x):xs) = do
  case x of
    VRet -> return ()
    Ret e -> throwErrorNoObj "Void function cannot return value"
    _ -> do 
      executeStatement x
      executeVoidFun xs

executeBlock :: Block -> ProgStateT ()
executeBlock (Block []) = return ()
executeBlock (Block (x:xs)) = do
  executeStatementOrFun x
  executeBlock (Block xs)

executeStatementOrFun :: StmtOrFn -> ProgStateT ()
executeStatementOrFun (StmtS s) = executeStatement s
executeStatementOrFun (StmtF f) = executeTopDef f

changeVarValue :: Ident -> Expr -> ProgStateT ()
changeVarValue i e = do
  evalExpr <- evaluateExpression e
  isDeclared <- isVarOrFuncDeclared i
  if isDeclared then do
    (stack,env,store,si) <- get
    varLoc <- getVarLoc i
    storeElem <- getStoreElem varLoc
    case storeElem of
      (VarSE (t,val)) -> do
        put (stack,env,Data.Map.insert varLoc (VarSE (t,(Expr evalExpr))) store,si)
    return ()
  else throwError "Variable not defined" i


executeStatement :: Stmt -> ProgStateT ()
executeStatement Empty = return ()
executeStatement (BStmt b) = executeBlock b
executeStatement (Decl dtype xs) = executeDeclaration dtype xs
executeStatement (ArrDecl atype ident arrbrs) = executeArrayDeclaration atype ident arrbrs
executeStatement (Ass ident expr) = executeVariableAssignment ident expr
executeStatement (AssArr ident arrbrs expr) = executeArrayAssignment ident arrbrs expr
executeStatement (Incr ident) = incrementVariable ident
executeStatement (Decr ident) = decrementVariable ident
executeStatement (Cond expr stmt) = executeConditional expr stmt
executeStatement (CondElse expr stmtif stmtelse) = executeConditionalElse expr stmtif stmtelse
executeStatement (While expr stmt) = executeWhile expr stmt
executeStatement Break = return () --does nothing if it's not in a while loop
executeStatement Continue = return () --does nothing if it's not in a while loop
executeStatement (SExp expr) = do 
  evaluateExpression expr
  return ()
executeStatement _ = return ()

getArrayElem :: ArrEE -> [Integer] -> ProgStateT Expr
getArrayElem (t,xs,sizes) inds = do
  isArrOk <- checkArraySize inds sizes
  if isArrOk then do
    findElemInArray xs inds
  else throwErrorNoObj "Wrong indices for array"

checkArraySize :: [Integer] -> [Integer] -> ProgStateT Bool
checkArraySize xs sizes = do
  if (length xs /= length sizes) then return False
  else do 
    checkMaxIndices xs sizes

checkMaxIndices :: [Integer] -> [Integer] -> ProgStateT Bool
checkMaxIndices [] [] = return True
checkMaxIndices (x:xs) (s:sizes) = do
  if x >= s then return False
  else checkMaxIndices xs sizes

findElemInArray :: [([Integer],Expr)] -> [Integer] -> ProgStateT Expr
findElemInArray [] ys = throwErrorNoObj "Element in array not initialized"
findElemInArray ((xs,val):elems) ys = do
  if xs == ys then 
    return val
  else (findElemInArray elems ys)

deleteElemInArray :: [([Integer],Expr)] -> [Integer] -> ProgStateT [([Integer],Expr)]
deleteElemInArray arr inds = do
  let result = [l | l@(xs,exp) <- arr, xs /= inds] in return result

executeArrayAssignment :: Ident -> [ArrBr] -> Expr -> ProgStateT ()
executeArrayAssignment ident arrbrs expr = do
  envElem <- getEnvElem ident
  case envElem of
    ArrEE (t,xs,sizes) -> do
      indeces <- getArrSizes arrbrs
      isArrOk <- checkArraySize indeces sizes
      if isArrOk then do
        value <- evaluateExpression expr
        arrWithoutVal <- deleteElemInArray xs indeces --deleting value assigned under those indices
        (stack,env,store,si) <- get
        put (stack,Data.Map.insert ident (ArrEE (t,((indeces,value):arrWithoutVal),sizes)) env,store,si) --putting an array woth a new value
      else throwError "Wrong indeces for array" ident
    _ -> throwError "Array not declared" ident

executeArrayDeclaration :: Type -> Ident -> [ArrBr] -> ProgStateT ()
executeArrayDeclaration atype ident exs = do
  arrSizes <- getArrSizes exs
  declareFunOrArr ident (ArrEE (atype,[],arrSizes))

getArrSizes :: [ArrBr] -> ProgStateT [Integer]
getArrSizes [] = return []
getArrSizes ((ArrBrac x):xs) = do
  index <- evaluateNumericExpression x
  arr <- getArrSizes xs
  return ((index:[]) ++ arr)

executeVariableAssignment :: Ident -> Expr -> ProgStateT ()
executeVariableAssignment i (Lambda ltype args block) = do
  isDeclared <- isVarOrFuncDeclared i
  case isDeclared of
    True -> do
      LambdaEE (ltype,types,args,lblock) <- getEnvElem i
      (stack,env,store,si) <- get
      let l _ = Just (LambdaEE (ltype,types,args,(LambdaBlock block))) in
        put (stack,Data.Map.update l i env,store,si)
    False -> throwError ("Lambda variable not declared") i
executeVariableAssignment i expr = changeVarValue i expr

executeDeclaration :: Type -> [Item] -> ProgStateT ()
executeDeclaration _ [] = return ()
executeDeclaration dtype@(Func ftype argtypes) (x:xs) = do --lambdas
  case x of
    NoInit ident -> do
      declareFunOrArr ident (LambdaEE (ftype,argtypes,[],NonInit))
    Init ident (Lambda ltype args block) -> do
      declareFunOrArr ident (LambdaEE (ftype,argtypes,args,(LambdaBlock block)))
  executeDeclaration dtype xs
executeDeclaration dtype (x:xs) = do
  case x of
    NoInit ident -> do
      declareVariable ident (VarSE (dtype,NotInst))
    Init ident expr -> do
      evalExpr <- evaluateExpression expr
      declareVariable ident (VarSE (dtype,(Expr evalExpr)))
  executeDeclaration dtype xs

executeWhile :: Expr -> Stmt -> ProgStateT ()
executeWhile expr stmt = do
  bool <- evaluateBoolExpression expr
  if bool then do
    case stmt of
      BStmt (Block xs) -> do
        shouldContinue <- executeWhileBlockStatement xs
        if shouldContinue then executeWhile expr stmt else return ()
      _ -> do
        executeStatement stmt
        executeWhile expr stmt
  else return ()

executeWhileBlockStatement :: [StmtOrFn] -> ProgStateT Bool --return False if break appears
executeWhileBlockStatement [] = return True
executeWhileBlockStatement ((StmtS x):xs) = do
  case x of
    Break -> return False
    Continue -> return True
    BStmt (Block xs) -> executeWhileBlockStatement xs
    _ -> do
      executeStatement x
      executeWhileBlockStatement xs
executeWhileBlockStatement ((StmtF x):xs) = return False

executeConditionalElse :: Expr -> Stmt -> Stmt -> ProgStateT ()
executeConditionalElse expr stmtif stmtelse = do
  exprVal <- evaluateBoolExpression expr
  if exprVal then executeStatement stmtif
  else executeStatement stmtelse

executeConditional :: Expr -> Stmt -> ProgStateT ()
executeConditional expr stmt = do
  exprVal <- evaluateBoolExpression expr
  if exprVal then executeStatement stmt
  else return ()

decrementVariable :: Ident -> ProgStateT ()
decrementVariable i = do
  val <- getVarExpr i
  case val of
    ELitInt integ -> changeVarValue i (ELitInt (integ - 1))
    _ -> throwError "Variable is not and integer and cannot be decremented" i

incrementVariable :: Ident -> ProgStateT ()
incrementVariable i = do
  val <- getVarExpr i
  case val of
    ELitInt integ -> changeVarValue i (ELitInt (integ + 1))
    _ -> throwError "Variable is not and integer and cannot be incremented" i

evaluateBoolExpression :: Expr -> ProgStateT Bool
evaluateBoolExpression ELitTrue = return True
evaluateBoolExpression ELitFalse = return False
evaluateBoolExpression (EVar ident) = do
  varExpr <- getVarExpr ident
  case varExpr of
    ELitTrue -> return True
    ELitFalse -> return False
    _ -> throwError "Variable is not a boolean" ident
evaluateBoolExpression (Not expr) = do
  result <- evaluateBoolExpression expr
  return (not result)
evaluateBoolExpression (EAnd expr1 expr2) = do
  result1 <- evaluateBoolExpression expr1
  result2 <- evaluateBoolExpression expr2
  return (result1 && result2)
evaluateBoolExpression (EOr expr1 expr2) = do
  result1 <- evaluateBoolExpression expr1
  result2 <- evaluateBoolExpression expr2
  return (result1 || result2)
evaluateBoolExpression e@(ERel expr1 relop expr2) = evaluateRelExpression e
evaluateBoolExpression e@(EArr ident arrbrs) = do
  expr <- evaluateArrayExpression ident arrbrs
  evaluateBoolExpression expr
evaluateBoolExpression e = throwError "Expression is not a boolean" e

evaluateRelExpression :: Expr -> ProgStateT Bool
evaluateRelExpression (ERel expr1 relop expr2) = do
  result1 <- evaluateNumericExpression expr1
  result2 <- evaluateNumericExpression expr2
  case relop of
    LTH -> return (result1 < result2)
    LE -> return (result1 <= result2)
    GTH -> return (result1 > result2)
    GE -> return (result1 >= result2)
    EQU -> return (result1 == result2)
    NE -> return (result1 /= result2)
evaluateRelExpression o = throwError "Expression is not a rel expression" o

evaluateNumericExpression :: Expr -> ProgStateT Integer
evaluateNumericExpression (ELitInt e) = return e
evaluateNumericExpression (EVar ident) = do
  varExpr <- getVarExpr ident
  case varExpr of
    (ELitInt i) -> return i
    _ -> throwError "Variable is not an integer" ident
evaluateNumericExpression (Neg e) = do
  expr <- evaluateNumericExpression e
  return $ negate expr
evaluateNumericExpression e@(EMul expr1 mulOp expr2) = evaluateEMulExpression e
evaluateNumericExpression e@(EAdd expr1 mulOp expr2) = evaluateEAddExpression e
evaluateNumericExpression e@(EArr ident arrbrs) = do
  expr <- evaluateArrayExpression ident arrbrs
  case expr of
    ELitInt i -> return i
    _ -> throwError "Array is not of integer type" ident
evaluateNumericExpression e = throwError "Expression is not of integer type" e

evaluateEMulExpression :: Expr -> ProgStateT Integer
evaluateEMulExpression (EMul expr1 mulOp expr2) = do
  result1 <- evaluateNumericExpression expr1
  result2 <- evaluateNumericExpression expr2
  case mulOp of
    Times -> return (result1 * result2)
    Div -> return (result1 `div` result2)
    Mod -> return (result1 `mod` result2)
evaluateEMulExpression o = throwError "Expression is not a mul expression" o

evaluateEAddExpression :: Expr -> ProgStateT Integer
evaluateEAddExpression (EAdd expr1 addOp expr2) = do
  result1 <- evaluateNumericExpression expr1
  result2 <- evaluateNumericExpression expr2
  case addOp of
    Plus -> return (result1 + result2)
    Minus -> return (result1 - result2)
evaluateEAddExpression o = throwError "Expression is not an add expression" o


evaluateExpression :: Expr -> ProgStateT Expr
evaluateExpression e@(Lambda ltype [arg] block) = return e
evaluateExpression e@(EAdd expr1 addOp expr2) = do
  result <- evaluateNumericExpression e
  return (ELitInt result)
evaluateExpression e@(EMul expr1 mulOp expr2) = do
  result <- evaluateNumericExpression e
  return (ELitInt result)
evaluateExpression e@(ERel expr1 relOp expr2) = do
  result <- evaluateBoolExpression e
  case result of
    True -> return ELitTrue
    False -> return ELitFalse
evaluateExpression e@(EAnd expr1 expr2) = do
  result <- evaluateBoolExpression e
  case result of
    True -> return ELitTrue
    False -> return ELitFalse
evaluateExpression e@(EOr expr1 expr2) = do
  result <- evaluateBoolExpression e
  case result of
    True -> return ELitTrue
    False -> return ELitFalse
evaluateExpression e@ELitTrue = return e
evaluateExpression e@ELitFalse = return e
evaluateExpression e@(Neg expr) = do
  result <- evaluateNumericExpression e
  return (ELitInt result)
evaluateExpression e@(ELitInt i) = return e
evaluateExpression e@(EString s) = return e
evaluateExpression e@(Not expr) = do
  result <- evaluateBoolExpression e
  case result of
    True -> return ELitTrue
    False -> return ELitFalse
evaluateExpression (EArr ident arrbrs) = evaluateArrayExpression ident arrbrs
evaluateExpression (EApp ident exprs) = do
  ret <- callFun ident exprs
  case ret of
    Just r -> return r
    Nothing -> return ELitTrue
evaluateExpression (EVar ident) = getVarExpr ident

evaluateArrayExpression :: Ident -> [ArrBr] -> ProgStateT Expr
evaluateArrayExpression ident arrbrs = do
  envElem <- getEnvElem ident
  case envElem of
    ArrEE e@(t,xs,sizes) -> do
      indeces <- getArrSizes arrbrs
      getArrayElem e indeces
    _ -> throwError "Array is not declared" ident 

executeTopDef :: TopDef -> ProgStateT ()
executeTopDef f = return ()

declareFunOrArr :: Ident -> EnvElem -> ProgStateT ()
declareFunOrArr i ee = do
  (stack,env,store,si) <- get
  put (stack,Data.Map.insert i ee env,store,si)

declareVariable :: Ident -> StoreElem -> ProgStateT ()
declareVariable i se = do
  loc <- allocateMemory i se
  (stack,env,store,si) <- get
  put (stack,Data.Map.insert i (VarEE loc) env,store,si)

allocateMemory :: Ident -> StoreElem -> ProgStateT Loc
allocateMemory i se = do
  (stack,env,store,si) <- get
  put (stack,env,Data.Map.insert si se store,si+1)
  return si

isVarOrFuncDeclared :: Ident -> ProgStateT Bool
isVarOrFuncDeclared i = do
  (_,env,_,_) <- get
  if Data.Map.member i env then return True
  else return False


getEnvElem :: Ident -> ProgStateT EnvElem
getEnvElem i = do
  (_,env,_,_) <- get 
  case Data.Map.lookup i env of
    Just el -> return el
    Nothing -> throwError "Env element not declared" i

getStoreElem :: Loc -> ProgStateT StoreElem
getStoreElem l = do
  (_,_,store,_) <- get 
  case Data.Map.lookup l store of
    Just el -> return el
    Nothing -> throwError "Store element not in scope" l

getVarExpr :: Ident -> ProgStateT Expr
getVarExpr i = do
  envElem <- getEnvElem i
  case envElem of 
    VarEE l -> do
      VarSE (vtype,sexpr) <- getStoreElem l
      case sexpr of
        Expr e -> do
          return e
        NotInst -> throwError "Variable not initialized" i
    FunEE f -> return ELitFalse

getVarLoc :: Ident -> ProgStateT Loc
getVarLoc i = do
  ee <- getEnvElem i
  case ee of 
    VarEE l -> return l
    FunEE f -> throwError "Object is a function, not a variable" i















