module TypesMiLatte where

import AbsMiLatte
import Control.Monad.State
import Data.Map (Map)

type ProgStateT = StateT ProgContext IO

type ProgContext = (Stack,Env,Store,StoreIndex)

type Stack = [StackElem]
type Env = Data.Map.Map Ident EnvElem
type Store = Data.Map.Map Loc StoreElem
type Loc = Integer --memory address
type StoreIndex = Integer

data StackElem = StackElem deriving Show
data EnvElem = VarEE VarEE | FunEE FunEE | ArrEE ArrEE | LambdaEE LambdaEE deriving Show
data StoreElem = VarSE VarSE deriving Show

type VarEE = Loc
type FunEE = (Type,[Arg],Block)
type ArrEE = (Type,ArrImpl,ArrSize)
type ArrImpl = [([Integer],Expr)]
type ArrSize = [Integer]

type LambdaEE = (Type,[Type],[Arg],LBlock)
data LBlock = LambdaBlock Block | NonInit deriving Show

type VarSE = (Type,SExpr)
data SExpr =  Expr Expr | NotInst deriving Show

--TYPES VALIDATOR
type ValidStateT = StateT ValidatorContext IO
type ValidatorContext = VEnv

type VEnv = Data.Map.Map Ident VEnvElem
type VEnvElem = (Type,ElemType)
data ElemType = VarV | ArrV | FunV FunV | LambdaV
type FunV = ([Arg],Block)
type LambdaV = ([Arg],Block)