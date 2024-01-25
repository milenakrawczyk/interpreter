module AbsMiLatte where

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Ident [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type Ident
  deriving (Eq, Ord, Show, Read)

data Block = Block [StmtOrFn]
  deriving (Eq, Ord, Show, Read)

data StmtOrFn = StmtS Stmt | StmtF TopDef
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | ArrDecl Type Ident [ArrBr]
    | Ass Ident Expr
    | AssArr Ident [ArrBr] Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | Break
    | Continue
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data ArrBr = ArrBrac Expr
  deriving (Eq, Ord, Show, Read)

data Type = Int | Str | Bool | Void | Arr Type | Func Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = Lambda Type [Arg] Block
    | EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | EArr Ident [ArrBr]
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

