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
import Language.Python.Version3.Parser.Token as Token hiding (True, False)
import Language.Python.Version3.Parser.ParserMonad hiding (location)
import Language.Python.Data.SrcLocation 
import Data.List (foldl')

makeConditionalExpr :: ExprSpan -> Maybe (ExprSpan, ExprSpan) -> ExprSpan
makeConditionalExpr e Nothing = e
makeConditionalExpr e opt@(Just (cond, false_branch))
   = CondExpr e cond false_branch (spanning e opt)

makeBinOp :: ExprSpan -> [(OpSpan, ExprSpan)] -> ExprSpan
makeBinOp e es
   = foldl' mkOp e es
   where
   mkOp e1 (op, e2) = BinaryOp op e1 e2 (spanning e1 e2)

parseError :: Token -> P a 
parseError token 
   = failP (getSpan token) ["Unexpected token", show token] 

data Trailer
   = TrailerCall { trailer_call_args :: [ArgumentSpan], trailer_span :: SrcSpan }
   | TrailerSubscript { trailer_subs :: [Subscript], trailer_span :: SrcSpan }
   | TrailerDot { trailer_dot_ident :: IdentSpan, dot_span :: SrcSpan, trailer_span :: SrcSpan }

instance Span Trailer where
  getSpan = trailer_span

data Subscript
   = SubscriptExpr { subscript_expr :: ExprSpan, subscript_span :: SrcSpan }
   | SubscriptSlice 
     { subscript_slice_span1 :: Maybe ExprSpan
     , subscript_slice_span2 :: Maybe ExprSpan
     , subscript_slice_span3 :: Maybe (Maybe ExprSpan)
     , subscript_span :: SrcSpan
     }

instance Span Subscript where
   getSpan = subscript_span

isProperSlice :: Subscript -> Bool
isProperSlice (SubscriptSlice {}) = True
isProperSlice other = False

subscriptToSlice :: Subscript -> SliceSpan
subscriptToSlice (SubscriptSlice lower upper stride span)
   = SliceProper lower upper stride span
subscriptToSlice (SubscriptExpr e span)
   = SliceExpr e span

subscriptToExpr :: Subscript -> ExprSpan
subscriptToExpr sub@(SubscriptExpr {}) = subscript_expr sub 
-- this should never happen:
subscriptToExpr (SubscriptSlice {}) 
   = error "subscriptToExpr applied to a proper slice"

{-
   = TrailerCall { trailer_call_args :: [ArgumentSpan], trailer_span :: SrcSpan }
   | TrailerSubscript { trailer_subs :: [Subscript], trailer_span :: SrcSpan }
   | TrailerDot { trailer_dot_ident :: IdentSpan, dot_span :: SrcSpan, trailer_span :: SrcSpan }
-}

addTrailer :: ExprSpan -> [Trailer] -> ExprSpan
addTrailer
   = foldl' trail
   where
   trail :: ExprSpan -> Trailer -> ExprSpan
   -- XXX fix the span
   trail e trail@(TrailerCall { trailer_call_args = args }) = Call e args (spanning e trail)
   trail e trail@(TrailerSubscript { trailer_subs = subs })
      | any isProperSlice subs
           = SlicedExpr e (map subscriptToSlice subs) (spanning e trail) 
      | otherwise 
           = Subscript e (map subscriptToExpr subs) (spanning e trail) 
   trail e trail@(TrailerDot { trailer_dot_ident = ident, dot_span = ds })
      = BinaryOp (AST.Dot ds) e (Var ident (getSpan ident)) (spanning e trail)

makeTupleOrExpr :: [ExprSpan] -> Maybe Token -> ExprSpan
makeTupleOrExpr [e] Nothing = e
makeTupleOrExpr es@(_:_) (Just t) = Tuple es (spanning es t) 
makeTupleOrExpr es@(_:_) Nothing  = Tuple es (getSpan es)

makeAssignmentOrExpr :: ExprSpan -> Either [ExprSpan] (AssignOpSpan, ExprSpan) -> StatementSpan
makeAssignmentOrExpr e (Left es) 
   = makeNormalAssignment e es
   where
   makeNormalAssignment :: ExprSpan -> [ExprSpan] -> StatementSpan
   makeNormalAssignment e [] = StmtExpr e (getSpan e)
   makeNormalAssignment e es 
      = AST.Assign (e : front) (head back) (spanning e es)
      where
      (front, back) = splitAt (len - 1) es
      len = length es 
makeAssignmentOrExpr e1 (Right (op, e2)) 
   = makeAugAssignment e1 op e2
   where
   makeAugAssignment :: ExprSpan -> AssignOpSpan -> ExprSpan -> StatementSpan
   makeAugAssignment e1 op e2
      = AST.AugmentedAssign e1 op e2 (spanning e1 e2)

makeTry :: Token -> SuiteSpan -> ([HandlerSpan], [StatementSpan], [StatementSpan]) -> StatementSpan
makeTry t1 body (handlers, elses, finally)
   = AST.Try body handlers elses finally 
     (spanning (spanning (spanning (spanning t1 body) handlers) elses) finally)

makeParam :: (IdentSpan, Maybe ExprSpan) -> Maybe ExprSpan -> ParameterSpan
makeParam (name, annot) defaultVal
   = Param name annot defaultVal paramSpan
   where
   paramSpan = spanning (spanning name annot) defaultVal

makeStarParam :: Token -> Maybe (IdentSpan, Maybe ExprSpan) -> ParameterSpan
makeStarParam t1 Nothing = EndPositional (getSpan t1) 
makeStarParam t1 (Just (name, annot))
   = VarArgsPos name annot (spanning t1 annot) 

makeStarStarParam :: Token -> (IdentSpan, Maybe ExprSpan) -> ParameterSpan
makeStarStarParam t1 (name, annot)
   = VarArgsKeyword name annot (spanning (spanning t1 name) annot) 

makeComprehension :: ExprSpan -> CompForSpan -> ComprehensionSpan ExprSpan
makeComprehension e for = Comprehension e for (spanning e for)

makeListForm :: SrcSpan -> Either ExprSpan (ComprehensionSpan ExprSpan) -> ExprSpan
makeListForm span (Left tuple@(Tuple {})) = List (tuple_exprs tuple) span
makeListForm span (Left other) = List [other] span 
makeListForm span (Right comprehension) = ListComp comprehension span

makeSet :: ExprSpan -> Either CompForSpan [ExprSpan] -> SrcSpan -> ExprSpan
makeSet e (Left compFor) = SetComp (Comprehension e compFor (spanning e compFor))
makeSet e (Right es) = Set (e:es)

makeDictionary :: (ExprSpan, ExprSpan) -> Either CompForSpan [(ExprSpan,ExprSpan)] -> SrcSpan -> ExprSpan
makeDictionary e (Left compFor) = DictComp (Comprehension e compFor (spanning e compFor))
makeDictionary e (Right es) = Dictionary (e:es)

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

makeDecorator :: Token -> DottedNameSpan -> [ArgumentSpan] -> DecoratorSpan
makeDecorator t1 name [] = Decorator name [] (spanning t1 name)
makeDecorator t1 name args = Decorator name args (spanning t1 args)

-- parser guarantees that the first list is non-empty
makeDecorated :: [DecoratorSpan] -> StatementSpan -> StatementSpan
makeDecorated ds@(d:_) def = Decorated ds def (spanning d def)

-- suite can't be empty so it is safe to take span over it
makeFun :: Token -> IdentSpan -> [ParameterSpan] -> Maybe ExprSpan -> SuiteSpan -> StatementSpan
makeFun t1 name params annot body = 
   Fun name params annot body $ spanning t1 body 

makeReturn :: Token -> Maybe ExprSpan -> StatementSpan
makeReturn t1 Nothing = AST.Return Nothing (getSpan t1)
makeReturn t1 expr@(Just e) = AST.Return expr (spanning t1 e)

makeParenOrGenerator :: Either ExprSpan (ComprehensionSpan ExprSpan) -> SrcSpan -> ExprSpan
makeParenOrGenerator (Left e) span = Paren e span
makeParenOrGenerator (Right comp) span = Generator comp span
   
{-
makeRelative :: Int -> ImportRelativeSpan -> SrcSpan -> ImportRelativeSpan
makeRelative dots importRelative span
   = importRelative { import_relative_dots = dots + oldDots, import_relative_annot = span }
   where
   oldDots = import_relative_dots importRelative
-}

makeRelative :: [Either Token DottedNameSpan] -> ImportRelativeSpan
makeRelative items =
   ImportRelative ndots maybeName (getSpan items) 
   where
   (ndots, maybeName) = countDots 0 items
   -- parser ensures that the dotted name will be at the end 
   -- of the list if it is there at all
   countDots :: Int -> [Either Token DottedNameSpan] -> (Int, Maybe DottedNameSpan)
   countDots count [] = (count, Nothing)
   countDots count (Right name:_) = (count, Just name)
   countDots count (Left token:rest) = countDots (count + dots token) rest 
   dots (Token.Dot {}) = 1
   dots (Token.Ellipsis {}) = 3

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

checkArguments :: [ArgumentSpan] -> P [ArgumentSpan]
checkArguments args = do
   check 1 args
   return args
   where
   check :: Int -> [ArgumentSpan] -> P ()
   check state [] = return ()
   check 5 (arg:_) = failP (getSpan arg) ["an **argument must not be followed by any other arguments"]
   check state (arg:rest) = do
      case arg of
         ArgExpr {}
            | state == 1 -> check state rest
            | state == 2 -> failP (getSpan arg) ["a positional argument must not follow a keyword argument"]
            | otherwise -> failP (getSpan arg) ["a positional argument must not follow a *argument"]
         ArgKeyword {}
            | state `elem` [1,2] -> check 2 rest
            | state `elem` [3,4] -> check 4 rest
         ArgVarArgsPos {}
            | state `elem` [1,2] -> check 3 rest
            | state `elem` [3,4] -> failP (getSpan arg) ["there must not be two *arguments in an argument list"]
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

checkParameters :: [ParameterSpan] -> P [ParameterSpan]
checkParameters params = do
   check 1 params 
   return params
   where
   check :: Int -> [ParameterSpan] -> P ()
   check state [] = return ()
   check 4 (p:_) = failP (getSpan p) ["a **parameter must not be followed by any other parameters"]
   check state (param:rest) = do
      case param of
         Param {}
            | state `elem` [1,3] -> check state rest
            | state == 2 -> check 3 rest 
         EndPositional {}
            | state == 1 -> check 2 rest
            | otherwise -> failP (getSpan param) ["there must not be two *parameters in a parameter list"]
         VarArgsPos {}
            | state == 1 -> check 2 rest
            | otherwise -> failP (getSpan param) ["there must not be two *parameters in a parameter list"]
         VarArgsKeyword {} -> check 4 rest
