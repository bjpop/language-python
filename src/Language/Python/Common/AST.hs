{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version2.Syntax.AST 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Representation of the Python version 2 abstract syntax. 
--
-- See: 
--
-- * <http://www.python.org/doc/2.6/reference/index.html> for an overview of the language. 
--
-- * <http://www.python.org/doc/2.6/reference/grammar.html> for the full grammar.
--
-- Note: there are cases where the AST is more liberal than the formal grammar
-- of the language. Therefore some care must be taken when constructing
-- Python programs using the raw AST. XXX At some point we should provide
-- smart constructors which ensure syntactic correctness of the AST.
-----------------------------------------------------------------------------

module Language.Python.Common.AST ( 
   -- * Annotation projection
     Annotated (..)
   -- * Modules
   , Module (..), ModuleSpan
   -- * Identifiers and dotted names
   , Ident (..), IdentSpan
   , DottedName, DottedNameSpan
   -- * Statements, suites, parameters, decorators and assignment operators
   , Statement (..), StatementSpan
   , Suite, SuiteSpan
   , Parameter (..), ParameterSpan
   , ParamTuple (..), ParamTupleSpan
   , Decorator (..), DecoratorSpan
   , AssignOp (..), AssignOpSpan
   -- * Expressions, operators, arguments and slices
   , Expr (..), ExprSpan
   , Op (..), OpSpan
   , Argument (..), ArgumentSpan
   , Slice (..), SliceSpan
   -- * Imports
   , ImportItem (..), ImportItemSpan
   , FromItem (..), FromItemSpan
   , FromItems (..), FromItemsSpan
   , ImportRelative (..), ImportRelativeSpan
   -- * Exceptions
   , Handler (..), HandlerSpan
   , ExceptClause (..), ExceptClauseSpan
   , RaiseExpr (..), RaiseExprSpan
   -- * Comprehensions
   , Comprehension (..), ComprehensionSpan
   , CompFor (..), CompForSpan
   , CompIf (..), CompIfSpan
   , CompIter (..), CompIterSpan
   )
   where

import Language.Python.Common.SrcLocation ( Span (getSpan), SrcSpan (..) ) 
import Data.Data

--------------------------------------------------------------------------------

class Annotated t where
   annot :: t annot -> annot

-- | Identifier.
data Ident annot = Ident { ident_string :: !String, ident_annot :: annot }
   deriving (Eq,Ord,Show,Typeable,Data)

-- | Identifier, annotated with source span.
type IdentSpan = Ident SrcSpan

instance Span IdentSpan where
   getSpan = annot 

instance Annotated Ident where
   annot = ident_annot

-- | A module (Python source file). See <http://www.python.org/doc/3.0/reference/toplevel_components.html>.
newtype Module annot = Module [Statement annot] -- ^ A module is just a sequence of top-level statements.
   deriving (Eq,Ord,Show,Typeable,Data)

-- | Module, annotated with source span.
type ModuleSpan = Module SrcSpan

-- | A block of statements. A suite is a group of statements controlled by a clause, 
-- for example, the body of a loop. See <http://www.python.org/doc/3.0/reference/compound_stmts.html>.
type Suite annot = [Statement annot] 

-- | Suite, annotated with source span.
type SuiteSpan = Suite SrcSpan

-- | A compound name constructed with the dot operator.
type DottedName annot = [Ident annot]

-- | Dotted Name, annotated with source span.
type DottedNameSpan = DottedName SrcSpan 

-- | An entity imported using the \'import\' keyword.
-- See <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-import-statement>.
data ImportItem annot = 
   ImportItem 
   { import_item_name :: DottedName annot -- ^ The name of module to import.
   , import_as_name :: Maybe (Ident annot)  -- ^ An optional name to refer to the entity (the \'as\' name). 
   , import_item_annot :: annot
   }
   deriving (Eq,Ord,Show,Typeable,Data)

type ImportItemSpan = ImportItem SrcSpan

instance Span ImportItemSpan where
   getSpan = annot 

instance Annotated ImportItem where
   annot = import_item_annot 

-- | An entity imported using the \'from ... import\' construct.
-- See <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-import-statement>
data FromItem annot = 
   FromItem 
   { from_item_name :: Ident annot -- ^ The name of the entity imported. 
   , from_as_name :: Maybe (Ident annot) -- ^ An optional name to refer to the entity (the \'as\' name).
   , from_item_annot :: annot
   }
   deriving (Eq,Ord,Show,Typeable,Data)

type FromItemSpan = FromItem SrcSpan

instance Span FromItemSpan where
   getSpan = annot 

instance Annotated FromItem where
   annot = from_item_annot 

-- | Items imported using the \'from ... import\' construct.
data FromItems annot 
   = ImportEverything { from_items_annot :: annot } -- ^ Import everything exported from the module.
   | FromItems { from_items_items :: [FromItem annot], from_items_annot :: annot } -- ^ Import a specific list of items from the module.
   deriving (Eq,Ord,Show,Typeable,Data)

type FromItemsSpan = FromItems SrcSpan

instance Span FromItemsSpan where
   getSpan = annot 

instance Annotated FromItems where
   annot = from_items_annot 

-- | A reference to the module to import from using the \'from ... import\' construct.
data ImportRelative annot 
   = ImportRelative 
     { import_relative_dots :: Int
     , import_relative_module :: Maybe (DottedName annot) 
     , import_relative_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type ImportRelativeSpan = ImportRelative SrcSpan

instance Span ImportRelativeSpan where
  getSpan = annot 

instance Annotated ImportRelative where
   annot = import_relative_annot 

-- | Statements.
--
--  See:
--
-- * <http://www.python.org/doc/3.0/reference/simple_stmts.html>
--
-- * <http://www.python.org/doc/3.0/reference/compound_stmts.html>
data Statement annot 
   -- | Import statement.
   = Import 
     { import_items :: [ImportItem annot] -- ^ Items to import.
     , stmt_annot :: annot 
     } 
   -- | From ... import statement.
   | FromImport 
     { from_module :: ImportRelative annot -- ^ Module to import from.
     , from_items :: FromItems annot -- ^ Items to import.
     , stmt_annot :: annot
     }
   -- | While loop. See <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-while-statement>.
   | While 
     { while_cond :: Expr annot -- ^ Loop condition.
     , while_body :: Suite annot -- ^ Loop body.
     , while_else :: Suite annot -- ^ Else clause.
     , stmt_annot :: annot
     }
   -- | For loop. See <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-for-statement>.
   | For 
     { for_targets :: [Expr annot] -- ^ Loop variables.
     , for_generator :: Expr annot -- ^ Loop generator. 
     , for_body :: Suite annot -- ^ Loop body
     , for_else :: Suite annot -- ^ Else clause.
     , stmt_annot :: annot
     }
   -- | Function definition. See <http://www.python.org/doc/3.0/reference/compound_stmts.html#function-definitions>.
   | Fun 
     { fun_name :: Ident annot -- ^ Function name.
     , fun_args :: [Parameter annot] -- ^ Function parameter list.
     , fun_result_annotation :: Maybe (Expr annot) -- ^ Optional result annotation.
     , fun_body :: Suite annot -- ^ Function body.
     , stmt_annot :: annot 
     }
   -- | Class definition. See <http://www.python.org/doc/3.0/reference/compound_stmts.html#class-definitions>.
   | Class 
     { class_name :: Ident annot -- ^ Class name.
     , class_args :: [Argument annot] -- ^ Class argument list. In version 2.x this is only ArgExprs. 
     , class_body :: Suite annot -- ^ Class body.
     , stmt_annot :: annot
     }
   -- | Conditional statement (if-elif-else). See <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-if-statement>.  
   | Conditional 
     { cond_guards :: [(Expr annot, Suite annot)] -- ^ Sequence of if-elif conditional clauses.
     , cond_else :: Suite annot -- ^ Possibly empty unconditional else clause.
     , stmt_annot :: annot
     }
   -- | Assignment statement. See <http://www.python.org/doc/3.0/reference/simple_stmts.html#assignment-statements>.
   | Assign 
     { assign_to :: [Expr annot] -- ^ Entity to assign to. XXX perhaps this should not be a list.
     , assign_expr :: Expr annot -- ^ Expression to evaluate.
     , stmt_annot :: annot
     }
   -- | Augmented assignment statement. See <http://www.python.org/doc/3.0/reference/simple_stmts.html#augmented-assignment-statements>.
   | AugmentedAssign 
     { aug_assign_to :: Expr annot -- ^ Entity to assign to.
     , aug_assign_op :: AssignOp annot -- ^ Assignment operator (for example \'+=\').
     , aug_assign_expr :: Expr annot  -- ^ Expression to evaluate.
     , stmt_annot :: annot
     }
   -- | Decorated definition of a function or class.
   | Decorated 
     { decorated_decorators :: [Decorator annot] -- ^ Decorators.
     , decorated_def :: Statement annot -- ^ Function or class definition to be decorated.
     , stmt_annot :: annot 
     }
   -- | Return statement (may only occur syntactically nested in a function definition). See <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-return-statement>.
   | Return 
     { return_expr :: Maybe (Expr annot) -- ^ Optional expression to evaluate and return to caller.
     , stmt_annot :: annot 
     }
   -- | Try statement (exception handling). See <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-try-statement>.
   | Try 
     { try_body :: Suite annot -- ^ Try clause.
     , try_excepts :: [Handler annot] -- ^ Exception handlers.
     , try_else :: Suite annot -- ^ Possibly empty else clause, executed if and when control flows off the end of the try clause.
     , try_finally :: Suite annot -- ^ Possibly empty finally clause.
     , stmt_annot :: annot
     }
   -- | Raise statement (exception throwing). See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-raise-statement>
   | Raise 
     { raise_expr :: RaiseExpr annot 
     , stmt_annot :: annot
     }
   -- | With statement (context management). See <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-with-statement>. And also see: <http://www.python.org/dev/peps/pep-0343/>.
   | With 
     { with_context :: [(Expr annot, Maybe (Expr annot))] -- ^ Context expression(s) (yields a context manager).
     , with_body :: Suite annot -- ^ Suite to be managed.
     , stmt_annot :: annot
     }
   -- | Pass statement (null operation). See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-pass-statement>
   | Pass { stmt_annot :: annot }
   -- | Break statement (may only occur syntactically nested in a for or while loop, but not nested in a function or class definition within that loop). See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-break-statement>.
   | Break { stmt_annot :: annot }
   -- | Continue statement (may only occur syntactically nested in a for or while loop, but not nested in a function or class definition or finally clause within that loop). See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-continue-statement>.
   | Continue { stmt_annot :: annot }
   -- | Del statement (delete). See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-del-statement>. 
   | Delete 
     { del_exprs :: [Expr annot] -- ^ Items to delete.
     , stmt_annot :: annot 
     }
   -- | Expression statement. See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#expression-statements>. 
   | StmtExpr { stmt_expr :: Expr annot, stmt_annot :: annot }
   -- | Global declaration. See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-global-statement>. 
   | Global 
     { global_vars :: [Ident annot] -- ^ Variables declared global in the current block.
     , stmt_annot :: annot
     }
   -- | Nonlocal declaration. See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-nonlocal-statement>.
   | NonLocal 
     { nonLocal_vars :: [Ident annot] -- ^ Variables declared nonlocal in the current block (their binding comes from bound the nearest enclosing scope).
     , stmt_annot :: annot
     }
   -- | Assertion. See: <http://www.python.org/doc/3.0/reference/simple_stmts.html#the-assert-statement>.
   | Assert 
     { assert_exprs :: [Expr annot] -- ^ Expressions being asserted.
     , stmt_annot :: annot
     }
   -- | Print statement. Version 2 only. See <http://docs.python.org/reference/simple_stmts.html#the-print-statement>
   | Print 
     { print_chevron :: Bool -- ^ Optional chevron (>>)
     , print_exprs :: [Expr annot] -- ^ Arguments to print
     , print_trailing_comma :: Bool -- ^ Does it end in a comma?
     , stmt_annot :: annot 
     }
   -- | Exec statement. Version 2 only. See <http://docs.python.org/reference/simple_stmts.html#grammar-token-exec_stmt> 
   | Exec
     { exec_expr :: Expr annot
     , exec_globals_locals :: Maybe (Expr annot, Maybe (Expr annot))
     , stmt_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

data RaiseExpr annot
   = RaiseV3 (Maybe (Expr annot, Maybe (Expr annot))) -- ^ Optional expression to evaluate, and optional \'from\' clause.
   | RaiseV2 (Maybe (Expr annot, (Maybe (Expr annot, Maybe (Expr annot)))))
   deriving (Eq,Ord,Show,Typeable,Data)

type RaiseExprSpan = RaiseExpr SrcSpan

type StatementSpan = Statement SrcSpan

instance Span StatementSpan where
   getSpan = annot 

instance Annotated Statement where
   annot = stmt_annot 

-- | Decorator.
data Decorator annot = 
   Decorator 
   { decorator_name :: DottedName annot -- ^ Decorator name.
   , decorator_args :: [Argument annot] -- ^ Decorator arguments.
   , decorator_annot :: annot 
   }
   deriving (Eq,Ord,Show,Typeable,Data)

type DecoratorSpan = Decorator SrcSpan

instance Span DecoratorSpan where
   getSpan = annot 

instance Annotated Decorator where
   annot = decorator_annot 

-- | Formal parameter of function definitions and lambda expressions.
-- 
-- See:
--
-- * <http://www.python.org/doc/3.0/reference/compound_stmts.html#function-definitions>
--
-- * <http://www.python.org/doc/3.0/reference/expressions.html#calls>
data Parameter annot
   -- | Ordinary named parameter.
   = Param 
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_default :: Maybe (Expr annot) -- ^ Optional default value.
     , param_annot :: annot
     }
   -- | Excess positional parameter (single asterisk before its name in the concrete syntax). 
   | VarArgsPos 
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_annot :: annot
     }
   -- | Excess keyword parameter (double asterisk before its name in the concrete syntax).
   | VarArgsKeyword 
     { param_name :: Ident annot -- ^ Parameter name.
     , param_py_annotation :: Maybe (Expr annot) -- ^ Optional annotation.
     , param_annot :: annot
     }
   -- | Marker for the end of positional parameters (not a parameter itself).
   | EndPositional { param_annot :: annot }
   -- | Tuple unpack. Version 2 only.
   | UnPackTuple 
     { param_unpack_tuple :: ParamTuple annot
     , param_default :: Maybe (Expr annot)
     , param_annot :: annot
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type ParameterSpan = Parameter SrcSpan

instance Span ParameterSpan where
  getSpan = annot 

instance Annotated Parameter where
   annot = param_annot 

data ParamTuple annot
   = ParamTupleName { param_tuple_name :: Ident annot, param_tuple_annot :: annot }
   | ParamTuple { param_tuple :: [ParamTuple annot], param_tuple_annot :: annot } 
   deriving (Eq,Ord,Show,Typeable,Data)

type ParamTupleSpan = ParamTuple SrcSpan

instance Span ParamTupleSpan where
   getSpan = annot

instance Annotated ParamTuple where
   annot = param_tuple_annot

-- | Arguments to function calls, class declarations and decorators.
data Argument annot
   -- | Ordinary argument expression.
   = ArgExpr { arg_expr :: Expr annot, arg_annot :: annot }
   -- | Excess positional argument.
   | ArgVarArgsPos { arg_expr :: Expr annot, arg_annot :: annot }
   -- | Excess keyword argument.
   | ArgVarArgsKeyword { arg_expr :: Expr annot, arg_annot :: annot }
   -- | Keyword argument.
   | ArgKeyword 
     { arg_keyword :: Ident annot -- ^ Keyword name.
     , arg_expr :: Expr annot -- ^ Argument expression.
     , arg_annot :: annot
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type ArgumentSpan = Argument SrcSpan

instance Span ArgumentSpan where
  getSpan = annot 

instance Annotated Argument where
   annot = arg_annot 

-- | Exception handler. See: <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-try-statement>.
data Handler annot
   = Handler 
     { handler_clause :: ExceptClause annot
     , handler_suite :: Suite annot
     , handler_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type HandlerSpan = Handler SrcSpan

instance Span HandlerSpan where
   getSpan = annot 

instance Annotated Handler where
   annot = handler_annot 

-- | Exception clause. See: <http://www.python.org/doc/3.0/reference/compound_stmts.html#the-try-statement>.
data ExceptClause annot
   = ExceptClause 
     -- NB: difference with version 3 (has NAME as target, but looks like bug in grammar)
     { except_clause :: Maybe (Expr annot, Maybe (Expr annot))
     , except_clause_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type ExceptClauseSpan = ExceptClause SrcSpan

instance Span ExceptClauseSpan where
   getSpan = annot 

instance Annotated ExceptClause where
   annot = except_clause_annot 

-- | Comprehension. See: <http://www.python.org/doc/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries> 
data Comprehension e annot
   = Comprehension 
     { comprehension_expr :: e
     , comprehension_for :: CompFor annot
     , comprehension_annot :: annot 
     }
   deriving (Eq,Ord,Show,Typeable,Data)

type ComprehensionSpan e = Comprehension e SrcSpan

instance Span (ComprehensionSpan e) where
   getSpan = annot 

instance Annotated (Comprehension e) where
   annot = comprehension_annot 

-- | Comprehension \'for\' component. See: <http://www.python.org/doc/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>
data CompFor annot = 
   CompFor 
   { comp_for_exprs :: [Expr annot]
   , comp_in_expr :: Expr annot
   , comp_for_iter :: Maybe (CompIter annot) 
   , comp_for_annot :: annot
   }
   deriving (Eq,Ord,Show,Typeable,Data)

type CompForSpan = CompFor SrcSpan

instance Span CompForSpan where
   getSpan = annot 

instance Annotated CompFor where
   annot = comp_for_annot 

-- | Comprehension guard. See: <http://www.python.org/doc/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>.
data CompIf annot = 
   CompIf 
   { comp_if :: Expr annot
   , comp_if_iter :: Maybe (CompIter annot)
   , comp_if_annot :: annot 
   }
   deriving (Eq,Ord,Show,Typeable,Data)

type CompIfSpan = CompIf SrcSpan

instance Span CompIfSpan where
   getSpan = annot 

instance Annotated CompIf where
   annot = comp_if_annot 

-- | Comprehension iterator (either a \'for\' or an \'if\'). See: <http://www.python.org/doc/3.0/reference/expressions.html#displays-for-lists-sets-and-dictionaries>.
data CompIter annot 
   = IterFor { comp_iter_for :: CompFor annot, comp_iter_annot :: annot }
   | IterIf { comp_iter_if :: CompIf annot, comp_iter_annot :: annot }
   deriving (Eq,Ord,Show,Typeable,Data)

type CompIterSpan = CompIter SrcSpan

instance Span CompIterSpan where
   getSpan = annot 

instance Annotated CompIter where
   annot = comp_iter_annot 

-- | Expression.
-- 
-- See: <http://www.python.org/doc/3.0/reference/expressions.html>.
data Expr annot
   -- | Variable.
   = Var { var_ident :: Ident annot, expr_annot :: annot }
   -- | Literal integer.
   | Int { int_value :: Integer, expr_literal :: String, expr_annot :: annot }
   -- | Long literal integer. Version 2 only.
   | LongInt { int_value :: Integer, expr_literal :: String, expr_annot :: annot }
   -- | Literal floating point number.
   | Float { float_value :: Double, expr_literal :: String, expr_annot :: annot }
   -- | Literal imaginary number.
   | Imaginary { imaginary_value :: Double, expr_literal :: String, expr_annot :: annot } 
   -- | Literal boolean.
   | Bool { bool_value :: Bool, expr_annot :: annot }
   -- | Literal \'None\' value.
   | None { expr_annot :: annot } 
   -- | Ellipsis \'...\'.
   | Ellipsis { expr_annot :: annot }
   -- | Literal byte string.
   | ByteStrings { byte_string_strings :: [String], expr_annot :: annot }
   -- | Literal strings (to be concatentated together).
   | Strings { strings_strings :: [String], expr_annot :: annot }
   -- | Function call. See: <http://www.python.org/doc/3.0/reference/expressions.html#calls>.
   | Call 
     { call_fun :: Expr annot -- ^ Expression yielding a callable object (such as a function).
     , call_args :: [Argument annot] -- ^ Call arguments.
     , expr_annot :: annot
     }
   -- | Subscription, for example \'x [y]\'. See: <http://www.python.org/doc/3.0/reference/expressions.html#id5>.
   | Subscript { subscriptee :: Expr annot, subscript_exprs :: [Expr annot], expr_annot :: annot }
   -- | Slicing, for example \'w [x:y:z]\'. See: <http://www.python.org/doc/3.0/reference/expressions.html#id6>.
   | SlicedExpr { slicee :: Expr annot, slices :: [Slice annot], expr_annot :: annot } 
   -- | Conditional expresison. See: <http://www.python.org/doc/3.0/reference/expressions.html#boolean-operations>. 
   | CondExpr 
     { ce_true_branch :: Expr annot -- ^ Expression to evaluate if condition is True.
     , ce_condition :: Expr annot -- ^ Boolean condition.
     , ce_false_branch :: Expr annot -- ^ Expression to evaluate if condition is False.
     , expr_annot :: annot
     }
   -- | Binary operator application.
   | BinaryOp { operator :: Op annot, left_op_arg :: Expr annot, right_op_arg :: Expr annot, expr_annot :: annot }
   -- | Unary operator application.
   | UnaryOp { operator :: Op annot, op_arg :: Expr annot, expr_annot :: annot }
   -- | Anonymous function definition (lambda). See: <http://www.python.org/doc/3.0/reference/expressions.html#id15>.
   | Lambda { lambda_args :: [Parameter annot], lambda_body :: Expr annot, expr_annot :: annot }
   -- | N-ary tuple of arity greater than 0. The list should not be empty.
   | Tuple { tuple_exprs :: [Expr annot], expr_annot :: annot }
   -- | Generator yield. See: <http://www.python.org/doc/3.0/reference/expressions.html#yield-expressions>.
   | Yield 
     { yield_expr :: Maybe (Expr annot) -- ^ Optional expression to yield.
     , expr_annot :: annot
     }
   -- | Generator. See: <http://www.python.org/doc/3.0/reference/expressions.html#generator-expressions>.
   | Generator { gen_comprehension :: Comprehension (Expr annot) annot, expr_annot :: annot }
   -- | List comprehension. See: <http://www.python.org/doc/3.0/reference/expressions.html#list-displays>.
   | ListComp { list_comprehension :: Comprehension (Expr annot) annot, expr_annot :: annot }
   -- | List. See: <http://www.python.org/doc/3.0/reference/expressions.html#list-displays>.
   | List { list_exprs :: [Expr annot], expr_annot :: annot }
   -- | Dictionary. See: <http://www.python.org/doc/3.0/reference/expressions.html#dictionary-displays>.
   | Dictionary { dict_mappings :: [(Expr annot, Expr annot)], expr_annot :: annot }
   -- | Dictionary comprehension. See: <http://www.python.org/doc/3.0/reference/expressions.html#dictionary-displays>.
   | DictComp { dict_comprehension :: Comprehension (Expr annot, Expr annot) annot, expr_annot :: annot }
   -- | Set. See: <http://www.python.org/doc/3.0/reference/expressions.html#set-displays>.
   | Set { set_exprs :: [Expr annot], expr_annot :: annot } 
   -- | Set comprehension. <http://www.python.org/doc/3.0/reference/expressions.html#set-displays>.
   | SetComp { set_comprehension :: Comprehension (Expr annot) annot, expr_annot :: annot }
   -- | Starred expression. 
   | Starred { starred_expr :: Expr annot, expr_annot :: annot }
   -- | Parenthesised expression.
   | Paren { paren_expr :: Expr annot, expr_annot :: annot }
   -- | String conversion (backquoted expression). Version 2 only. See: <http://docs.python.org/reference/expressions.html#grammar-token-string_conversion>.
   | StringConversion { backquoted_expr :: Expr annot, expr_anot :: annot }
   deriving (Eq,Ord,Show,Typeable,Data)

type ExprSpan = Expr SrcSpan

instance Span ExprSpan where
   getSpan = annot 

instance Annotated Expr where
   annot = expr_annot 

data Slice annot
   = SliceProper 
     { slice_lower :: Maybe (Expr annot)
     , slice_upper :: Maybe (Expr annot)
     , slice_stride :: Maybe (Maybe (Expr annot)) 
     , slice_annot :: annot
     } 
   | SliceExpr 
     { slice_expr :: Expr annot
     , slice_annot :: annot 
     }
   | SliceEllipsis { slice_annot :: annot }
   deriving (Eq,Ord,Show,Typeable,Data)

type SliceSpan = Slice SrcSpan

instance Span SliceSpan where
   getSpan = annot 

instance Annotated Slice where
   annot = slice_annot 

-- | Operators.
data Op annot
   = And { op_annot :: annot } -- ^ \'and\'
   | Or { op_annot :: annot } -- ^ \'or\'
   | Not { op_annot :: annot } -- ^ \'not\'
   | Exponent { op_annot :: annot } -- ^ \'**\'
   | LessThan { op_annot :: annot } -- ^ \'<\'
   | GreaterThan { op_annot :: annot } -- ^ \'>\'
   | Equality { op_annot :: annot } -- ^ \'==\'
   | GreaterThanEquals { op_annot :: annot } -- ^ \'>=\'
   | LessThanEquals { op_annot :: annot } -- ^ \'<=\'
   | NotEquals  { op_annot :: annot } -- ^ \'!=\'
   | NotEqualsV2  { op_annot :: annot } -- ^ \'<>\'. Version 2 only.
   | In { op_annot :: annot } -- ^ \'in\'
   | Is { op_annot :: annot } -- ^ \'is\'
   | IsNot { op_annot :: annot } -- ^ \'is not\'
   | NotIn { op_annot :: annot } -- ^ \'not in\'
   | BinaryOr { op_annot :: annot } -- ^ \'|\'
   | Xor { op_annot :: annot } -- ^ \'^\'
   | BinaryAnd { op_annot :: annot } -- ^ \'&\'
   | ShiftLeft { op_annot :: annot } -- ^ \'<<\'
   | ShiftRight { op_annot :: annot } -- ^ \'>>\'
   | Multiply { op_annot :: annot } -- ^ \'*\'
   | Plus { op_annot :: annot } -- ^ \'+\'
   | Minus { op_annot :: annot } -- ^ \'-\'
   | Divide { op_annot :: annot } -- ^ \'\/\'
   | FloorDivide { op_annot :: annot } -- ^ \'\/\/\'
   | Invert { op_annot :: annot } -- ^ \'~\' (bitwise inversion of its integer argument)
   | Modulo { op_annot :: annot } -- ^ \'%\'
   | Dot { op_annot :: annot } -- ^ \'.\'
   deriving (Eq,Ord,Show,Typeable,Data)

type OpSpan = Op SrcSpan

instance Span OpSpan where
  getSpan = annot 

instance Annotated Op where
   annot = op_annot 

-- | Augmented assignment operators.
data AssignOp annot
   = PlusAssign { assignOp_annot :: annot } -- ^ \'+=\'
   | MinusAssign { assignOp_annot :: annot } -- ^ \'-=\'
   | MultAssign { assignOp_annot :: annot } -- ^ \'*=\'
   | DivAssign { assignOp_annot :: annot } -- ^ \'\/=\'
   | ModAssign { assignOp_annot :: annot } -- ^ \'%=\'
   | PowAssign { assignOp_annot :: annot } -- ^ \'*=\'
   | BinAndAssign { assignOp_annot :: annot } -- ^ \'&=\'
   | BinOrAssign { assignOp_annot :: annot } -- ^ \'|=\'
   | BinXorAssign { assignOp_annot :: annot } -- ^ \'^=\' 
   | LeftShiftAssign { assignOp_annot :: annot } -- ^ \'<<=\'
   | RightShiftAssign { assignOp_annot :: annot } -- ^ \'>>=\'
   | FloorDivAssign { assignOp_annot :: annot } -- ^ \'\/\/=\'
   deriving (Eq,Ord,Show,Typeable,Data)

type AssignOpSpan = AssignOp SrcSpan

instance Span AssignOpSpan where
   getSpan = annot 

instance Annotated AssignOp where
   annot = assignOp_annot 
