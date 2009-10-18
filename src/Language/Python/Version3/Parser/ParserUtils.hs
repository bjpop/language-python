{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Parser.ParserUtils 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Various utilities to support the Python parser. 
-----------------------------------------------------------------------------

module Language.Python.Version3.Parser.ParserUtils where

import Language.Python.Version3.Syntax.AST as AST
import Language.Python.Version3.Parser.Token hiding (True, False)
import Language.Python.Version3.Parser.ParserMonad hiding (location)
import Language.Python.Data.SrcLocation 
import Data.List (foldl')

makeConditionalExpr :: Expr -> Maybe (Expr, Expr) -> Expr
makeConditionalExpr e Nothing = e
makeConditionalExpr e (Just (cond, false_branch))
    = CondExpr
      { ce_true_branch = e
      , ce_condition = cond
      , ce_false_branch = false_branch
      }

makeBinOp :: Expr -> [(Op, Expr)] -> Expr
makeBinOp e es
   -- = foldl' mkOp e (reverse es)
   = foldl' mkOp e es
   where
   mkOp e1 (op, e2) = 
      BinaryOp { operator = op, left_op_arg = e1, right_op_arg = e2 }

parseError :: Token -> P a 
parseError token 
   = failP (location token) ["Unexpected token", show token] 

data Trailer
   = TrailerCall [Argument]
   | TrailerSubscript [Subscript] 
   | TrailerDot Ident

data Subscript
   = SubscriptExpr Expr
   | SubscriptSlice (Maybe Expr) (Maybe Expr) (Maybe (Maybe Expr)) 

isProperSlice :: Subscript -> Bool
isProperSlice (SubscriptSlice {}) = True
isProperSlice other = False

subscriptToSlice :: Subscript -> Slice
subscriptToSlice (SubscriptSlice lower upper stride)
   = SliceProper { slice_lower = lower, slice_upper = upper, slice_stride = stride }
subscriptToSlice (SubscriptExpr e)
   = SliceExpr { slice_expr = e }

subscriptToExpr :: Subscript -> Expr
subscriptToExpr (SubscriptExpr e) = e
-- this should never happen:
subscriptToExpr (SubscriptSlice {}) 
   = error "subscriptToExpr applied to a proper slice"

addTrailer :: Expr -> [Trailer] -> Expr
addTrailer
   = foldl' trail
   where
   trail :: Expr -> Trailer -> Expr
   trail e (TrailerCall args) = Call { call_fun = e, call_args = args }
   trail e (TrailerSubscript subs) 
      | any isProperSlice subs
           = SlicedExpr { slicee = e, slices = map subscriptToSlice subs }
      | otherwise 
           = Subscript { subscriptee = e, subscript_exprs = map subscriptToExpr subs }
   trail e (TrailerDot ident)
      = BinaryOp { operator = AST.Dot, left_op_arg = e, right_op_arg = Var ident }

makeTupleOrExpr :: [Expr] -> Bool -> Expr
makeTupleOrExpr [e] False = e
makeTupleOrExpr es@[e] True = Tuple { tuple_exprs = es }
makeTupleOrExpr es@(_:_) _ = Tuple { tuple_exprs = es } 

makeAssignmentOrExpr :: Expr -> Either [Expr] (AssignOp, Expr) -> Statement
makeAssignmentOrExpr e (Left es) 
   = makeNormalAssignment e es
   where
   makeNormalAssignment :: Expr -> [Expr] -> Statement
   makeNormalAssignment e [] = StmtExpr { stmt_expr = e } 
   makeNormalAssignment e es 
      = AST.Assign { assign_to = e : front, assign_expr = head back }
      where
      (front, back) = splitAt (len - 1) es
      len = length es 
makeAssignmentOrExpr e1 (Right (op, e2)) 
   = makeAugAssignment e1 op e2
   where
   makeAugAssignment :: Expr -> AssignOp -> Expr -> Statement
   makeAugAssignment e1 op e2
      = AST.AugmentedAssign { aug_assign_to = e1, aug_assign_op = op, aug_assign_expr = e2 } 

makeTry :: Suite -> ([Handler], [Statement], [Statement]) -> Statement
makeTry body (handlers, elses, finally)
   = AST.Try { try_body = body, try_excepts = handlers, try_else = elses, try_finally = finally }

makeParam :: (Ident, Maybe Expr) -> Maybe Expr -> Parameter
makeParam (name, annot) defaultVal
   = Param { param_name = name, param_annotation = annot, param_default = defaultVal }

makeStarParam :: Maybe (Ident, Maybe Expr) -> Parameter
makeStarParam Nothing = EndPositional
makeStarParam (Just (name, annot))
   = VarArgsPos { param_name = name, param_annotation = annot }

makeStarStarParam :: (Ident, Maybe Expr) -> Parameter
makeStarStarParam (name, annot)
   = VarArgsKeyword { param_name = name, param_annotation = annot }

makeComprehension :: Expr -> CompFor -> Comprehension Expr
makeComprehension e for
   = Comprehension { comprehension_expr = e, comprehension_for = for }

makeListForm :: Either Expr (Comprehension Expr) -> Expr
makeListForm (Left tuple@(Tuple {})) = List { list_exprs = tuple_exprs tuple }
makeListForm (Left other) = List { list_exprs = [other] }
makeListForm (Right comprehension) = ListComp { list_comprehension = comprehension }

makeSet :: Expr -> Either CompFor [Expr] -> Expr
makeSet e (Left compFor)
   = SetComp { set_comprehension = Comprehension { comprehension_expr = e, comprehension_for = compFor }}
makeSet e (Right es) = Set { set_exprs = e:es }

makeDictionary :: (Expr, Expr) -> Either CompFor [(Expr,Expr)] -> Expr
makeDictionary e (Left compFor)
   = DictComp { dict_comprehension = Comprehension { comprehension_expr = e, comprehension_for = compFor }}
makeDictionary e (Right es) = Dictionary { dict_mappings = e:es }

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

{-
   See: http://www.python.org/doc/3.0/reference/expressions.html#calls

   arglist: (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test]
                         |'**' test)

   (state 1) Positional arguments come first.
   (state 2) Then keyword arguments.
   (state 3) Then the single star form.
   (state 4) Then more keyword arguments (but no positional arguments).
   (state 5) Then the double star form.

XXX fixme: we need to include SrcLocations for the errors.
-}

checkArguments :: [Argument] -> P [Argument]
checkArguments args = do
   check 1 args
   return args
   where
   check :: Int -> [Argument] -> P ()
   check state [] = return ()
   check 5 (_:_) = failP NoLocation ["an **argument must not be followed by any other arguments"]
   check state (arg:rest) = do
      case arg of
         ArgExpr {}
            | state == 1 -> check state rest
            | state == 2 -> failP NoLocation ["a positional argument must not follow a keyword argument"]
            | otherwise -> failP NoLocation ["a positional argument must not follow a *argument"]
         ArgKeyword {}
            | state `elem` [1,2] -> check 2 rest
            | state `elem` [3,4] -> check 4 rest
         ArgVarArgsPos {}
            | state `elem` [1,2] -> check 3 rest
            | state `elem` [3,4] -> failP NoLocation ["there must not be two *arguments in an argument list"]
         ArgVarArgsKeyword {} -> check 5 rest

{-
   See: http://docs.python.org/3.1/reference/compound_stmts.html#grammar-token-parameter_list

   parameter_list ::=  (defparameter ",")*
                    (  "*" [parameter] ("," defparameter)*
                    [, "**" parameter]
                    | "**" parameter
                    | defparameter [","] )

   (state 1) Parameters first.
   (state 2) Then the single star (on its own or with parameter)
   (state 3) Then more parameters. 
   (state 4) Then the double star form.

XXX fixme: we need to include SrcLocations for the errors.
-}

checkParameters :: [Parameter] -> P [Parameter]
checkParameters params = do
   check 1 params 
   return params
   where
   check :: Int -> [Parameter] -> P ()
   check state [] = return ()
   check 4 (_:_) = failP NoLocation ["a **parameter must not be followed by any other parameters"]
   check state (param:rest) = do
      case param of
         Param {}
            | state `elem` [1,3] -> check state rest
            | state == 2 -> check 3 rest 
         EndPositional {}
            | state == 1 -> check 2 rest
            | otherwise -> failP NoLocation ["there must not be two *parameters in a parameter list"]
         VarArgsPos {}
            | state == 1 -> check 2 rest
            | otherwise -> failP NoLocation ["there must not be two *parameters in a parameter list"]
         VarArgsKeyword {} -> check 4 rest
