-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Syntax.Pretty 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Pretty printing of the Python version 3 abstract syntax. XXX not quite
-- complete.
-----------------------------------------------------------------------------

module Language.Python.Version3.Syntax.Pretty where

import Language.Python.Version3.Syntax.AST 

import Text.PrettyPrint as TextPP
import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------

-- | All types which can be transformed into a 'Doc'.
class Pretty a where
   pretty :: a -> Doc

-- | Transform values into strings.
prettyText :: Pretty a => a -> String
prettyText = render . pretty

-- | Conditionally wrap parentheses around an item.
parensIf :: Pretty a => (a -> Bool) -> a -> Doc
parensIf test x = if test x then parens $ pretty x else pretty x 

perhaps :: Pretty a => Maybe a -> Doc -> Doc
perhaps Nothing doc = empty
perhaps (Just {}) doc = doc 

-- | A list of things separated by commas.
commaList :: Pretty a => [a] -> Doc
commaList = hsep . punctuate comma . map pretty 

instance Pretty BS.ByteString where
   -- XXX should handle the escaping properly
   pretty b = text "b" <> text (show $ BS.unpack b)

instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Double where
   pretty = double

instance Pretty Bool where
  pretty True = text "True"
  pretty False = text "False"

instance Pretty a => Pretty (Maybe a) where
   pretty Nothing = empty
   pretty (Just x) = pretty x

prettyString :: String -> Doc
   -- XXX should handle the escaping properly
prettyString str = text (show str)

instance Pretty Module where
   -- pretty :: Module -> Doc 
   pretty (Module stmts) = vcat $ map pretty stmts 

instance Pretty Ident where
   pretty (Ident name) = text name

dot :: Doc
dot = char '.'

prettyDottedName :: DottedName -> Doc
prettyDottedName [] = empty
prettyDottedName [name] = pretty name
prettyDottedName (name:rest@(_:_))
   = pretty name <> dot <> prettyDottedName rest

instance Pretty ImportItem where
   pretty (ImportItem {import_item_name = name, import_as_name = asName})
      = prettyDottedName name <+> (maybe empty (\n -> text "as" <+> pretty n) asName)

instance Pretty FromItem where
   pretty (FromItem { from_item_name = name, from_as_name = asName })
      = pretty name <+> (maybe empty (\n -> text "as" <+> pretty n) asName) 

instance Pretty FromItems where
   pretty ImportEverything = char '*'
   pretty (FromItems [item]) = pretty item 
   pretty (FromItems items) = parens (commaList items)

instance Pretty ImportModule where
   pretty (ImportRelative importModule) = dot <> pretty importModule
   pretty ImportDot = dot
   pretty (ImportName dottedName) = prettyDottedName dottedName 

prettySuite :: [Statement] -> Doc
prettySuite stmts = vcat $ map pretty stmts 

optionalKeywordSuite :: String -> [Statement] -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts = text keyword <> colon $+$ indent (prettySuite stmts)

prettyArgList :: [Argument] -> Doc
prettyArgList = parens . commaList 

prettyOptionalArgList :: [Argument] -> Doc
prettyOptionalArgList [] = empty
prettyOptionalArgList list = parens $ commaList list

prettyGuards :: [(Expr, Suite)] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards)
   = text "elif" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
     prettyGuards guards

indent :: Doc -> Doc
indent doc = nest 4 doc

-- XXX is there a better way to do this?
blankLine :: Doc
blankLine = text []

instance Pretty Statement where
   -- pretty :: Statement -> Doc 
   pretty (Import { import_items = items}) = text "import" <+> commaList items 
   pretty stmt@(FromImport {})
      = text "from" <+> pretty (from_module stmt) <+> text "import" <+> pretty (from_items stmt)
   pretty stmt@(While {})
      = text "while" <+> pretty (while_cond stmt) <> colon $+$
        indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
   pretty stmt@(For {})
      = text "for" <+> commaList (for_targets stmt) <+> text "in" <+> pretty (for_generator stmt) <> colon $+$
        indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
   pretty stmt@(Fun {})
      = text "def" <+> pretty (fun_name stmt) <> parens (commaList (fun_args stmt)) <+> 
        perhaps (fun_result_annotation stmt) (text "->") <+>
        pretty (fun_result_annotation stmt) <> colon $+$ indent (prettySuite (fun_body stmt)) 
   pretty stmt@(Class {})
      = text "class" <+> pretty (class_name stmt) <> prettyOptionalArgList (class_args stmt) <> 
        colon $+$ indent (prettySuite (class_body stmt)) 
   pretty stmt@(Conditional { cond_guards = guards, cond_else = optionalElse })
      = case guards of
           (cond,body):xs -> 
              text "if" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$ 
              prettyGuards xs $+$
              optionalKeywordSuite "else" optionalElse
   -- XXX is the assign_to always a singleton?
   pretty (Assign { assign_to = pattern, assign_expr = e })
      = commaList pattern <+> equals <+> pretty e
   pretty (AugmentedAssign { aug_assign_to = to_expr, aug_assign_op = op, aug_assign_expr = e})
      = pretty to_expr <+> pretty op <+> pretty e 
   pretty (Decorated { decorated_decorators = decs, decorated_def = stmt})
      = vcat (map pretty decs) $+$ pretty stmt
   pretty (Return { return_expr = e }) = text "return" <+> pretty e
   pretty (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally})
      = text "try" <> colon $+$ indent (prettySuite body) $+$
        prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$ 
        optionalKeywordSuite "finally" finally 
   pretty (Raise { raise_expr = e })
      = text "raise" <+> 
        maybe empty (\ (x, fromE) -> pretty x <+> (maybe empty (\f -> text "from" <+> pretty f) fromE)) e
   pretty (With { with_context = context, with_body = body })
      = text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
        indent (prettySuite body)
   pretty Pass = text "pass"
   pretty Break = text "break"
   pretty Continue = text "continue"
   pretty (Delete { del_exprs = es }) = text "del" <+> commaList es
   pretty (StmtExpr { stmt_expr = e }) = pretty e
   pretty (Global { global_vars = idents }) = text "global" <+> commaList idents
   pretty (NonLocal { nonLocal_vars = idents }) = text "nonlocal" <+> commaList idents
   pretty (Assert { assert_exprs = es }) = text "assert" <+> commaList es

prettyWithContext :: (Expr, Maybe Expr) -> Doc
prettyWithContext (e, Nothing) = pretty e
prettyWithContext (e, Just as) = pretty e <+> text "as" <+> pretty as

prettyHandlers :: [Handler] -> Doc
prettyHandlers = foldr (\next rec -> prettyHandler next $+$ rec) empty

prettyHandler :: Handler -> Doc
prettyHandler (exceptClause, suite) 
   = text "except" <+> prettyExceptClause exceptClause <> colon $+$ indent (prettySuite suite)
prettyExceptClause :: ExceptClause -> Doc
prettyExceptClause Nothing = empty
prettyExceptClause (Just (e, target))
   = pretty e <+> maybe empty (\t -> text "as" <+> pretty t) target

instance Pretty Decorator where
   pretty (Decorator { decorator_name = name, decorator_args = args })
      = char '@' <> prettyDottedName name <+> prettyOptionalArgList args

instance Pretty Parameter where
   pretty (Param { param_name = ident, param_annotation = annot, param_default = def})
      = pretty ident <> (maybe empty (\e -> colon <> pretty e <> space) annot) <> 
        maybe empty (\e -> equals <> pretty e) def 
   pretty (VarArgsPos { param_name = ident, param_annotation = annot})
      = char '*' <> pretty ident <> (maybe empty (\e -> colon <> pretty e) annot)
   pretty (VarArgsKeyword { param_name = ident, param_annotation = annot })
      = text "**" <> pretty ident <> (maybe empty (\e -> colon <> pretty e) annot)
   pretty EndPositional = char '*' 

instance Pretty Argument where
   pretty (ArgExpr { arg_expr = e }) = pretty e
   pretty (ArgVarArgsPos { arg_expr = e}) = char '*' <> pretty e
   pretty (ArgVarArgsKeyword { arg_expr = e }) = text "**" <> pretty e
   pretty (ArgKeyword { arg_keyword = ident, arg_expr = e }) 
      = pretty ident <> equals <> pretty e

instance Pretty a => Pretty (Comprehension a) where
   pretty (Comprehension { comprehension_expr = e, comprehension_for = for }) 
      = pretty e <+> pretty for 

instance Pretty CompFor where
   pretty (CompFor { comp_for_exprs = es, comp_in_expr = e, comp_for_iter = iter }) 
      = text "for" <+> commaList es <+> text "in" <+> pretty e <+> pretty iter

instance Pretty CompIf where
   pretty (CompIf { comp_if = e, comp_if_iter = iter }) 
      = text "if" <+> pretty e <+> pretty iter 

instance Pretty CompIter where
   pretty (IterFor compFor) = pretty compFor 
   pretty (IterIf compIf) = pretty compIf

instance Pretty Expr where
   pretty (Var i) = pretty i
   pretty (Int i) = pretty i
   pretty (Float d) = pretty d
   pretty (Imaginary { imaginary_value = i }) = pretty i <> char 'j' 
   pretty (Bool b) = pretty b
   pretty None = text "None"
   pretty Ellipsis = text "..."
   pretty (ByteStrings bs) = hcat (map pretty bs)
   pretty (Strings ss) = hcat (map prettyString ss)
   pretty (Call { call_fun = f, call_args = args }) = pretty f <> prettyArgList args
   pretty (Subscript { subscriptee = e, subscript_exprs = subs })
      = pretty e <> brackets (commaList subs)
   pretty (SlicedExpr { slicee = e, slices = ss })
      = pretty e <> brackets (commaList ss) 
   pretty (CondExpr { ce_true_branch = trueBranch, ce_condition = cond, ce_false_branch = falseBranch })
      = pretty trueBranch <+> text "if" <+> pretty cond <+> text "else" <+> pretty falseBranch
   pretty (BinaryOp { operator = op, left_op_arg = left, right_op_arg = right })
      = pretty left <> (if op == Dot then dot else space <> pretty op <> space) <> pretty right
   pretty (UnaryOp { operator = op, op_arg = e }) = pretty op <+> pretty e
   pretty (Lambda { lambda_args = args, lambda_body = body })
      = text "lambda" <+> commaList args <> colon <+> pretty body
   pretty (Tuple { tuple_exprs = es }) = parens $ commaList es
   pretty (Yield { yield_expr = e })
      = text "yield" <+> pretty e
   pretty (List { list_exprs = es }) = brackets (commaList es)
   pretty (Dictionary { dict_mappings = mappings })
      = braces (hsep (punctuate comma $ map (\ (e1,e2) -> pretty e1 <> colon <> pretty e2) mappings))
   pretty (Set { set_exprs = es }) = braces $ commaList es
   pretty (ListComp { list_comprehension = lc }) = brackets $ pretty lc
   pretty (Generator { gen_comprehension = gc }) = parens $ pretty gc

instance Pretty Slice where
   pretty (SliceProper { slice_lower = lower, slice_upper = upper, slice_stride = stride })
      = pretty lower <> colon <> pretty upper <> (maybe empty (\s -> colon <> pretty s) stride)
   pretty (SliceExpr { slice_expr = e }) = pretty e

instance Pretty Op where
   pretty And = text "and"
   pretty Or = text "or"
   pretty Not = text "not"
   pretty Exponent = text "**"
   pretty LessThan = text "<"
   pretty GreaterThan = text ">"
   pretty Equality = text "=="
   pretty GreaterThanEquals = text ">="
   pretty LessThanEquals = text "<="
   pretty NotEquals = text "!="
   pretty In = text "in"
   pretty Is = text "is"
   pretty IsNot = text "is not"
   pretty NotIn = text "not in"
   pretty BinaryOr = text "|"
   pretty Xor = text "^"
   pretty BinaryAnd = text "&"
   pretty ShiftLeft = text "<<"
   pretty ShiftRight = text ">>"
   pretty Multiply = text "*"
   pretty Plus = text "+"
   pretty Minus = text "-"
   pretty Divide = text "/"
   pretty FloorDivide = text "//"
   pretty Invert = text "~"
   pretty Modulo = text "%"
   pretty Dot = dot

instance Pretty AssignOp where
   pretty PlusAssign = text "+="
   pretty MinusAssign = text "-="
   pretty MultAssign = text "*="
   pretty DivAssign = text "/="
   pretty ModAssign = text "%="
   pretty PowAssign = text "**="
   pretty BinAndAssign = text "&="
   pretty BinOrAssign = text "|="
   pretty BinXorAssign = text "^="
   pretty LeftShiftAssign = text "<<="
   pretty RightShiftAssign = text ">>="
   pretty FloorDivAssign = text "//="
