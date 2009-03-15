{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Syntax.AST 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Representation of the Python version 3 abstract syntax. 
--
-- See: 
--
-- * <http://docs.python.org/dev/3.0/reference/index.html> for an overview of the language. 
--
-- * <http://docs.python.org/dev/3.0/reference/grammar.html> for the full grammar.
--
-- Note: there are cases where the AST is more liberal than the formal grammar
-- of the language. Therefore some care must be taken when constructing
-- Python programs using the raw AST. XXX At some point we should provide
-- smart constructors which ensure syntactic correctness of the AST.
-----------------------------------------------------------------------------

module Language.Python.Version3.Syntax.AST ( 
   -- * Modules
   Module (..)
   -- * Identifiers and dotted names
   , Ident (..)
   , DottedName
   -- * Statements, suites, parameters, decorators and assignment operators
   , Statement (..)
   , Suite
   , Parameter (..)
   , Decorator (..)
   , AssignOp (..)
   -- * Expressions, operators, arguments and slices
   , Expr (..)
   , Op (..)
   , Argument (..)
   , Slice (..)
   -- * Imports
   , ImportItem (..)
   , FromItem (..)
   , FromItems (..)
   , ImportModule (..)
   -- * Exceptions
   , Handler
   , ExceptClause
   -- * Comprehensions
   , Comprehension (..)
   , CompFor (..)
   , CompIf (..)
   , CompIter (..)
   )
   where

import Language.Python.Version3.Parser.Token ( Ident (..) ) 
import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------

-- | A module (Python source file). See <http://docs.python.org/dev/3.0/reference/toplevel_components.html>.
newtype Module = Module [Statement] -- ^ A module is just a sequence of top-level statements.
   deriving Show

-- | A block of statements. A suite is a group of statements controlled by a clause, 
-- for example, the body of a loop. See <http://docs.python.org/dev/3.0/reference/compound_stmts.html>.
type Suite = [Statement] 

-- | A compound name constructed with the dot operator.
type DottedName = [Ident]

-- | An entity imported using the \'import\' keyword.
-- See <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-import-statement>.
data ImportItem = 
   ImportItem 
   { import_item_name :: DottedName -- ^ The name of module to import.
   , import_as_name :: Maybe Ident  -- ^ An optional name to refer to the entity (the \'as\' name). 
   }
   deriving Show

-- | An entity imported using the \'from ... import\' construct.
-- See <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-import-statement>
data FromItem = 
   FromItem 
   { from_item_name :: Ident -- ^ The name of the entity imported. 
   , from_as_name :: Maybe Ident -- ^ An optional name to refer to the entity (the \'as\' name).
   }
   deriving Show

-- | Items imported using the \'from ... import\' construct.
data FromItems 
   = ImportEverything -- ^ Import everything exported from the module.
   | FromItems [FromItem] -- ^ Import a specific list of items from the module.
   deriving Show

-- | A reference to the module to import from using the \'from ... import\' construct.
data ImportModule 
    = ImportRelative ImportModule  -- ^ Relative import. A dot followed by something.
    | ImportDot                    -- ^ Relative import. Dot on its own.
    | ImportName DottedName        -- ^ The name of the module to import from. 
    deriving Show

-- | Statements.
--
--  See:
--
-- * <http://docs.python.org/dev/3.0/reference/simple_stmts.html>
--
-- * <http://docs.python.org/dev/3.0/reference/compound_stmts.html>
data Statement 
   -- | Import statement.
   = Import { import_items :: [ImportItem] -- ^ Items to import.
     } 
   -- | From ... import statement.
   | FromImport 
     { from_module :: ImportModule -- ^ Module to import from.
     , from_items :: FromItems  -- ^ Items to import.
     }
   -- | While loop. See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-while-statement>.
   | While 
     { while_cond :: Expr -- ^ Loop condition.
     , while_body :: Suite -- ^ Loop body.
     , while_else :: Suite -- ^ Else clause.
     }
   -- | For loop. See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-for-statement>.
   | For 
     { for_targets :: [Expr] -- ^ Loop variables.
     , for_generator :: Expr -- ^ Loop generator. 
     , for_body :: Suite -- ^ Loop body
     , for_else :: Suite  -- ^ Else clause.
     }
   -- | Function definition. See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#function-definitions>.
   | Fun 
     { fun_name :: Ident -- ^ Function name.
     , fun_args :: [Parameter] -- ^ Function parameter list.
     , fun_result_annotation :: Maybe Expr -- ^ Optional result annotation.
     , fun_body :: Suite -- ^ Function body.
     }
   -- | Class definition. See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#class-definitions>.
   | Class 
     { class_name :: Ident -- ^ Class name.
     , class_args :: [Argument] -- ^ Class argument list.
     , class_body :: Suite -- ^ Class body.
     }
   -- | Conditional statement (if-elif-else). See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-if-statement>.  
   | Conditional 
     { cond_guards :: [(Expr, Suite)] -- ^ Sequence of if-elif conditional clauses.
     , cond_else :: Suite -- ^ Possibly empty unconditional else clause.
     }
   -- | Assignment statement. See <http://docs.python.org/dev/3.0/reference/simple_stmts.html#assignment-statements>.
   | Assign 
     { assign_to :: [Expr] -- ^ Entity to assign to. XXX perhaps this should not be a list.
     , assign_expr :: Expr -- ^ Expression to evaluate.
     }
   -- | Augmented assignment statement. See <http://docs.python.org/dev/3.0/reference/simple_stmts.html#augmented-assignment-statements>.
   | AugmentedAssign 
     { aug_assign_to :: Expr -- ^ Entity to assign to.
     , aug_assign_op :: AssignOp -- ^ Assignment operator (for example \'+=\').
     , aug_assign_expr :: Expr  -- ^ Expression to evaluate.
     }
   -- | Decorated definition of a function or class.
   | Decorated 
     { decorated_decorators :: [Decorator] -- ^ Decorators.
     , decorated_def :: Statement -- ^ Function or class definition to be decorated.
     }
   -- | Return statement (may only occur syntactically nested in a function definition). See <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-return-statement>.
   | Return 
     { return_expr :: Maybe Expr -- ^ Optional expression to evaluate and return to caller.
     }
   -- | Try statement (exception handling). See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-try-statement>.
   | Try 
     { try_body :: Suite -- ^ Try clause.
     , try_excepts :: [Handler] -- ^ Exception handlers.
     , try_else :: Suite -- ^ Possibly empty else clause, executed if and when control flows off the end of the try clause.
     , try_finally :: Suite -- ^ Possibly empty finally clause.
     }
   -- | Raise statement (exception throwing). See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-raise-statement>
   | Raise 
    { raise_expr :: Maybe (Expr, Maybe Expr) -- ^ Optional expression to evaluate, and optional \'from\' clause.
    }
   -- | With statement (context management). See <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-with-statement>. And also see: <http://www.python.org/dev/peps/pep-0343/>.
   | With 
     { with_context :: Expr -- ^ Context expression (yields a context manager).
     , with_as :: Maybe Expr -- ^ Optional target.
     , with_body :: Suite -- ^ Suite to be managed.
     }
   -- | Pass statement (null operation). See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-pass-statement>
   | Pass
   -- | Break statement (may only occur syntactically nested in a for or while loop, but not nested in a function or class definition within that loop). See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-break-statement>.
   | Break
   -- | Continue statement (may only occur syntactically nested in a for or while loop, but not nested in a function or class definition or finally clause within that loop). See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-continue-statement>.
   | Continue
   -- | Del statement (delete). See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-del-statement>. 
   | Delete 
     { del_exprs :: [Expr] -- ^ Items to delete.
     }
   -- | Expression statement. See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#expression-statements>. 
   | StmtExpr { stmt_expr :: Expr }
   -- | Global declaration. See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-global-statement>. 
   | Global 
     { global_vars :: [Ident] -- ^ Variables declared global in the current block.
     }
   -- | Nonlocal declaration. See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-nonlocal-statement>.
   | NonLocal 
     { nonLocal_vars :: [Ident] -- ^ Variables declared nonlocal in the current block (their binding comes from bound the nearest enclosing scope).
     }
   -- | Assertion. See: <http://docs.python.org/dev/3.0/reference/simple_stmts.html#the-assert-statement>.
   | Assert 
     { assert_exprs :: [Expr] -- ^ Expressions being asserted.
     }
   deriving Show

-- | Decorator.
data Decorator = 
   Decorator 
   { decorator_name :: DottedName -- ^ Decorator name.
   , decorator_args :: [Argument] -- ^ Decorator arguments.
   }
   deriving Show

-- | Formal parameter of function definitions and lambda expressions.
-- 
-- See:
--
-- * <http://docs.python.org/dev/3.0/reference/compound_stmts.html#function-definitions>
--
-- * <http://docs.python.org/dev/3.0/reference/expressions.html#calls>
data Parameter
   -- | Ordinary named parameter.
   = Param 
     { param_name :: Ident -- ^ Parameter name.
     , param_annotation :: Maybe Expr -- ^ Optional annotation.
     , param_default :: Maybe Expr -- ^ Optional default value.
     }
   -- | Excess positional parameter (single asterisk before its name in the concrete syntax). 
   | VarArgsPos 
     { param_name :: Ident -- ^ Parameter name.
     , param_annotation :: Maybe Expr -- ^ Optional annotation.
     }
   -- | Excess keyword parameter (double asterisk before its name in the concrete syntax).
   | VarArgsKeyword 
     { param_name :: Ident -- ^ Parameter name.
     , param_annotation :: Maybe Expr -- ^ Optional annotation.
     }
   -- | Marker for the end of positional parameters (not a parameter itself).
   | EndPositional 
   deriving Show

-- | Arguments to function calls, class declarations and decorators.
data Argument
   -- | Ordinary argument expression.
   = ArgExpr { arg_expr :: Expr }
   -- | Excess positional argument.
   | ArgVarArgsPos { arg_expr :: Expr }
   -- | Excess keyword argument.
   | ArgVarArgsKeyword { arg_expr :: Expr }
   -- | Keyword argument.
   | ArgKeyword 
     { arg_keyword :: Ident -- ^ Keyword name.
     , arg_expr :: Expr -- ^ Argument expression.
     }
   deriving Show 

-- | Exception handler. See: <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-try-statement>.
type Handler = (ExceptClause, Suite)
-- | Exception clause. See: <http://docs.python.org/dev/3.0/reference/compound_stmts.html#the-try-statement>.
type ExceptClause = Maybe (Expr, Maybe Ident)

-- | Comprehension. See: <http://docs.python.org/dev/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries> 
data Comprehension e
   = Comprehension { comprehension_expr :: e, comprehension_for :: CompFor }
   deriving Show

-- | Comprehension \'for\' component. See: <http://docs.python.org/dev/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>
data CompFor = CompFor { comp_for_exprs :: [Expr], comp_in_expr :: Expr, comp_for_iter :: Maybe CompIter }
   deriving Show

-- | Comprehension guard. See: <http://docs.python.org/dev/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>.
data CompIf = CompIf { comp_if :: Expr, comp_if_iter :: Maybe CompIter }
   deriving Show

-- | Comprehension iterator (either a \'for\' or an \'if\'). See: <http://docs.python.org/dev/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>.
data CompIter = IterFor CompFor | IterIf CompIf
   deriving Show

-- | Expression.
-- 
-- See: <http://docs.python.org/dev/3.0/reference/expressions.html>.
data Expr
   -- | Variable.
   = Var Ident
   -- | Literal integer.
   | Int Integer
   -- | Literal floating point number.
   | Float Double 
   -- | Literal imaginary number.
   | Imaginary { imaginary_value :: Double } 
   -- | Literal boolean.
   | Bool Bool
   -- | Literal \'None\' value.
   | None 
   -- | Ellipsis \'...\'.
   | Ellipsis
   -- | Literal byte string.
   | ByteStrings [BS.ByteString]
   -- | Literal strings (to be concatentated together).
   | Strings [String]
   -- | Function call. See: <http://docs.python.org/dev/3.0/reference/expressions.html#calls>.
   | Call 
     { call_fun :: Expr -- ^ Expression yielding a callable object (such as a function).
     , call_args :: [Argument] -- ^ Call arguments.
     }
   -- | Subscription, for example \'x [y]\'. See: <http://docs.python.org/dev/3.0/reference/expressions.html#id5>.
   | Subscript { subscriptee :: Expr, subscript_exprs :: [Expr] }
   -- | Slicing, for example \'w [x:y:z]\'. See: <http://docs.python.org/dev/3.0/reference/expressions.html#id6>.
   | SlicedExpr { slicee :: Expr, slices :: [Slice] } 
   -- | Conditional expresison. See: <http://docs.python.org/dev/3.0/reference/expressions.html#boolean-operations>. 
   | CondExpr 
     { ce_true_branch :: Expr -- ^ Expression to evaluate if condition is True.
     , ce_condition :: Expr -- ^ Boolean condition.
     , ce_false_branch :: Expr -- ^ Expression to evaluate if condition is False.
     }
   -- | Binary operator application.
   | BinaryOp { operator :: Op, left_op_arg :: Expr, right_op_arg :: Expr }
   -- | Unary operator application.
   | UnaryOp { operator :: Op, op_arg :: Expr }
   -- | Anonymous function definition (lambda). See: <http://docs.python.org/dev/3.0/reference/expressions.html#id15>.
   | Lambda { lambda_args :: [Parameter], lambda_body :: Expr }
   -- | N-ary tuple of arity greater than 0. The list should not be empty.
   | Tuple { tuple_exprs :: [Expr] }
   -- | Generator yield. See: <http://docs.python.org/dev/3.0/reference/expressions.html#yield-expressions>.
   | Yield 
     { yield_expr :: Maybe Expr -- ^ Optional expression to yield.
     }
   -- | Generator. See: <http://docs.python.org/dev/3.0/reference/expressions.html#generator-expressions>.
   | Generator { gen_comprehension :: Comprehension Expr }
   -- | List comprehension. See: <http://docs.python.org/dev/3.0/reference/expressions.html#list-displays>.
   | ListComp { list_comprehension :: Comprehension Expr }
   -- | List. See: <http://docs.python.org/dev/3.0/reference/expressions.html#list-displays>.
   | List { list_exprs :: [Expr] }
   -- | Dictionary. See: <http://docs.python.org/dev/3.0/reference/expressions.html#dictionary-displays>.
   | Dictionary { dict_mappings :: [(Expr, Expr)] }
   -- | Dictionary comprehension. See: <http://docs.python.org/dev/3.0/reference/expressions.html#dictionary-displays>.
   | DictComp { dict_comprehension :: Comprehension (Expr, Expr) }
   -- | Set. See: <http://docs.python.org/dev/3.0/reference/expressions.html#set-displays>.
   | Set { set_exprs :: [Expr] } 
   -- | Set comprehension. <http://docs.python.org/dev/3.0/reference/expressions.html#set-displays>.
   | SetComp { set_comprehension :: Comprehension Expr }
   -- | Starred expression. 
   | Starred { starred_expr :: Expr }
   deriving Show

data Slice
   = SliceProper { slice_lower :: Maybe Expr, slice_upper :: Maybe Expr, slice_stride :: Maybe (Maybe Expr) } 
   | SliceExpr { slice_expr :: Expr }
   deriving Show

-- | Operators.
data Op 
   = And -- ^ \'and\'
   | Or -- ^ \'or\'
   | Not -- ^ \'not\'
   | Exponent -- ^ \'**\'
   | LessThan -- ^ \'<\'
   | GreaterThan -- ^ \'>\'
   | Equality -- ^ \'==\'
   | GreaterThanEquals -- ^ \'>=\'
   | LessThanEquals -- ^ \'<=\'
   | NotEquals  -- ^ \'!=\'
   | In -- ^ \'in\'
   | Is -- ^ \'is\'
   | IsNot -- ^ \'is not\'
   | NotIn -- ^ \'not in\'
   | BinaryOr -- ^ \'|\'
   | Xor -- ^ \'^\'
   | BinaryAnd -- ^ \'&\'
   | ShiftLeft -- ^ \'<<\'
   | ShiftRight -- ^ \'>>\'
   | Multiply -- ^ \'*\'
   | Plus -- ^ \'+\'
   | Minus -- ^ \'-\'
   | Divide -- ^ \'\/\'
   | FloorDivide -- ^ \'\/\/\'
   | Invert -- ^ \'~\' (bitwise inversion of its integer argument)
   | Modulo -- ^ \'%\'
   | Dot -- ^ \'.\'
   deriving (Eq, Show)

-- | Augmented assignment operators.
data AssignOp
   = PlusAssign -- ^ \'+=\'
   | MinusAssign -- ^ \'-=\'
   | MultAssign -- ^ \'*=\'
   | DivAssign -- ^ \'\/=\'
   | ModAssign -- ^ \'%=\'
   | PowAssign -- ^ \'*=\'
   | BinAndAssign -- ^ \'&=\'
   | BinOrAssign -- ^ \'|=\'
   | BinXorAssign -- ^ \'^=\' 
   | LeftShiftAssign -- ^ \'<<=\'
   | RightShiftAssign -- ^ \'>>=\'
   | FloorDivAssign -- ^ \'\/\/=\'
   deriving (Eq, Show)
