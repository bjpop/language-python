module Language.Python.Version3.Syntax.AST 
   ( Program (..)
   , Statement (..)
   , Expr (..)
   , Op (..)
   , AssignOp (..)
   , Handler
   , ExceptClause
   , Suite
   , DottedName
   , Decorator (..)
   , Parameter (..)
   , Comprehension (..)
   , CompFor (..)
   , CompIf (..)
   , CompIter (..)
   , ImportItem (..)
   , FromItem (..)
   , FromItems (..)
   , ImportModule (..)
   , Slice (..)
   , Argument (..)
   , Ident (..)
   )
   where

import Language.Python.Version3.Parser.Token ( Ident (..) ) 
import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------

newtype Program = Program [Statement]
   deriving Show

type Suite = [Statement]

type DottedName = [Ident]

data ImportItem
   = ImportItem { import_item_name :: DottedName, import_as_name :: Maybe Ident }
   deriving Show

data FromItem
   = FromItem { from_item_name :: Ident, from_as_name :: Maybe Ident }
   deriving Show

data FromItems = ImportEverything | FromItems [FromItem]  
   deriving Show

data ImportModule 
    = ImportRelative ImportModule  -- dot followed by something 
    | ImportDot                    -- dot on its own
    | ImportName DottedName        -- the name of a module
    deriving Show

data Statement 
   = Import { import_items :: [ImportItem] } 
   | FromImport { from_module :: ImportModule, from_items :: FromItems }
   | While { while_cond :: Expr, while_body :: Suite, while_else :: Suite  }
   | For { for_targets :: [Expr], for_generator :: Expr, for_body :: Suite, for_else :: Suite }
   | Fun { fun_name :: Ident, fun_args :: [Parameter], fun_result_annotation :: Maybe Expr, fun_body :: Suite }
   | Class { class_name :: Ident, class_args :: [Argument], class_body :: Suite }
   | Conditional { cond_guards :: [(Expr, Suite)], cond_else :: Suite }
     -- XXX is the assign_to list always a singleton?
   | Assign { assign_to :: [Expr], assign_expr :: Expr }
   | AugmentedAssign { aug_assign_to :: Expr, aug_assign_op :: AssignOp, aug_assign_expr :: Expr }
   | Decorated { decorated_decorators :: [Decorator], {- Fun or Class -} decorated_def :: Statement }
   | Return { return_expr :: Maybe Expr }
   | Try { try_body :: Suite, try_excepts :: [Handler], try_else :: Suite, try_finally :: Suite }
   | Raise { raise_expr :: Maybe (Expr, Maybe Expr) }
   | With { with_context :: Expr, with_as :: Maybe Expr, with_body :: Suite }
   | Pass
   | Break
   | Continue
   | Delete { del_exprs :: [Expr] }
   | StmtExpr { stmt_expr :: Expr }
   | Global { global_vars :: [Ident] }
   | NonLocal { nonLocal_vars :: [Ident] }
   | Assert { assert_exprs :: [Expr] }
   deriving Show

data Decorator = Decorator { decorator_name :: DottedName, decorator_args :: [Argument] }
   deriving Show

data Parameter
   = Param { param_name :: Ident, param_annotation :: Maybe Expr, param_default :: Maybe Expr }
   | VarArgsPos { param_name :: Ident, param_annotation :: Maybe Expr }
   | VarArgsKeyword { param_name :: Ident, param_annotation :: Maybe Expr }
   | EndPositional {- Star on its own, not really a parameter but a marker -}
   deriving Show

data Argument
   = ArgExpr { arg_expr :: Expr }
   | ArgVarArgsPos { arg_expr :: Expr }
   | ArgVarArgsKeyword { arg_expr :: Expr }
   | ArgKeyword { arg_keyword :: Ident, arg_expr :: Expr }
   deriving Show 

type Handler = (ExceptClause, Suite)
type ExceptClause = Maybe (Expr, Maybe Ident)

data Comprehension e
   = Comprehension { comprehension_expr :: e, comprehension_for :: CompFor }
   deriving Show

data CompFor = CompFor { comp_for_exprs :: [Expr], comp_in_expr :: Expr, comp_for_iter :: Maybe CompIter }
   deriving Show

data CompIf = CompIf { comp_if :: Expr, comp_if_iter :: Maybe CompIter }
   deriving Show

data CompIter = IterFor CompFor | IterIf CompIf
   deriving Show

data Expr
   = Var Ident
   | Int Integer
   | Float Double 
   | Imaginary { imaginary_value :: Double } 
   | Bool Bool
   | None 
   | Ellipsis
   | ByteStrings [BS.ByteString]
   | Strings [String]
   | Call { call_fun :: Expr, call_args :: [Argument] }
   | Subscript { subscriptee :: Expr, subscript_exprs :: [Expr] }
   | SlicedExpr { slicee :: Expr, slices :: [Slice] } 
   | CondExpr { ce_true_branch :: Expr, ce_condition :: Expr, ce_false_branch :: Expr }
   | BinaryOp { operator :: Op, left_op_arg :: Expr, right_op_arg :: Expr }
   | UnaryOp { operator :: Op, op_arg :: Expr }
   | Lambda { lambda_args :: [Parameter], lambda_body :: Expr }
   | Tuple { tuple_exprs :: [Expr] }
   | Yield { yield_expr :: Maybe Expr }
   | Generator { gen_comprehension :: Comprehension Expr }
   | ListComp { list_comprehension :: Comprehension Expr }
   | List { list_exprs :: [Expr] }
   | Dictionary { dict_mappings :: [(Expr, Expr)] }
   | DictComp { dict_comprehension :: Comprehension (Expr, Expr) }
   | Set { set_exprs :: [Expr] } 
   | SetComp { set_comprehension :: Comprehension Expr }
   | Starred { starred_expr :: Expr }
   deriving Show

data Slice
   = SliceProper { slice_lower :: Maybe Expr, slice_upper :: Maybe Expr, slice_stride :: Maybe (Maybe Expr) } 
   | SliceExpr { slice_expr :: Expr }
   deriving Show

data Op 
   = And 
   | Or 
   | Not 
   | Exponent 
   | LessThan 
   | GreaterThan 
   | Equality
   | GreaterThanEquals
   | LessThanEquals
   | NotEquals 
   | In 
   | Is
   | IsNot
   | NotIn
   | BinaryOr
   | Xor
   | BinaryAnd
   | ShiftLeft
   | ShiftRight
   | Multiply
   | Plus
   | Minus
   | Divide
   | FloorDivide
   | Invert
   | Modulo
   | Dot
   deriving (Eq, Show)

data AssignOp
   = PlusAssign        -- +=
   | MinusAssign       -- -=
   | MultAssign        -- *=
   | DivAssign         -- /= 
   | ModAssign         -- %=
   | PowAssign         -- **=
   | BinAndAssign      -- &=
   | BinOrAssign       -- |=
   | BinXorAssign      -- ^=
   | LeftShiftAssign   -- <<=
   | RightShiftAssign  -- >>=
   | FloorDivAssign    -- //=
   deriving (Eq, Show)
