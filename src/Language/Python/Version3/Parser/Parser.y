{
module Language.Python.Version3.Parser.Parser where

import Data.Char (isSpace, isAlpha, isDigit)
import Language.Python.Version3.Parser.Lexer
import Language.Python.Version3.Parser.Token hiding (True, False)
import qualified Language.Python.Version3.Parser.Token 
import Language.Python.Version3.Syntax.AST
import Language.Python.Version3.Parser.ParserUtils
import Language.Python.Version3.Parser.ParserMonad
import Language.Python.SrcLocation
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS (ByteString)
}

%name parseFileInput FileInput 
%name parseSingleInput SingleInput
%name parseEval EvalInput  

%tokentype { Token } 
%error { parseError } 
%monad { P } { thenP } { returnP }
%lexer { lexCont } { Token.EOF }

%token 
   import          { Token.Import _ }
   ident           { Token.Identifier _ $$ }
   string          { Token.String _ $$ }
   bytestring      { Token.ByteString _ $$ }
   integer         { Token.Integer _ $$ }
   float           { Token.Float _ $$ }
   imaginary       { Token.Imaginary _ $$ }
   '='             { Token.Assign _ }
   '('             { Token.LeftRoundBracket _ }
   ')'             { Token.RightRoundBracket _ }
   '['             { Token.LeftSquareBracket _ }
   ']'             { Token.RightSquareBracket _ }
   '{'             { Token.LeftBrace _ }
   '}'             { Token.RightBrace _ }
   ','             { Token.Comma _ }
   ';'             { Token.SemiColon _ }
   ':'             { Token.Colon _ }
   def             { Token.Def _ }
   class           { Token.Class _ }
   while           { Token.While _ }
   for             { Token.For _ }
   if              { Token.If _ }
   with            { Token.With _ }
   as              { Token.As _ }
   true            { Token.True _ }
   false           { Token.False _ }
   none            { Token.None _ }
   return          { Token.Return _ }
   yield           { Token.Yield _ }
   indent          { Token.Indent _ }
   dedent          { Token.Dedent _ }
   newline         { Token.Newline _ }
   try             { Token.Try _ }
   except          { Token.Except _ }
   finally         { Token.Finally _ }
   raise           { Token.Raise _ }
   '+'             { Token.Plus _ }
   '-'             { Token.Minus _ }
   '*'             { Token.Mult _ }
   '/'             { Token.Div _ }
   '>'             { Token.GreaterThan _ }
   '<'             { Token.LessThan _ }
   '=='            { Token.Equality _ }
   '>='            { Token.GreaterThanEquals _ }
   '<='            { Token.LessThanEquals _ }
   and             { Token.And _ }
   or              { Token.Or _ }
   '**'            { Token.Exponent _ }
   pass            { Token.Pass _ }
   break           { Token.Break _ }
   continue        { Token.Continue _ }
   del             { Token.Delete _ }
   else            { Token.Else _ }
   elif            { Token.Elif _ }
   not             { Token.Not _ }
   '|'             { Token.BinaryOr _ }
   '^'             { Token.Xor _ }      
   '&'             { Token.BinaryAnd _ }      
   '>>'            { Token.ShiftLeft _ }
   '<<'            { Token.ShiftRight _ }
   '%'             { Token.Modulo _ }
   floordiv        { Token.FloorDiv _ }
   '~'             { Token.Tilde _ }
   '!='            { Token.NotEquals _ }
   in              { Token.In _ }
   is              { Token.Is _ }
   lambda          { Token.Lambda _ }
   '.'             { Token.Dot _ }
   '...'           { Token.Ellipsis _ }
   '+='            { Token.PlusAssign _ }
   '-='            { Token.MinusAssign _ }
   '*='            { Token.MultAssign _ }
   '/='            { Token.DivAssign _ }
   '%='            { Token.ModAssign _ }
   '**='           { Token.PowAssign _ }
   '&='            { Token.BinAndAssign _ }
   '|='            { Token.BinOrAssign _ }
   '^='            { Token.BinXorAssign _ }
   '<<='           { Token.LeftShiftAssign _ }
   '>>='           { Token.RightShiftAssign _ }
   '//='           { Token.FloorDivAssign _ } 
   '@'             { Token.At _ }
   '->'            { Token.RightArrow _ }
   from            { Token.From _ }
   global          { Token.Global _ }
   nonlocal        { Token.NonLocal _ }
   assert          { Token.Assert _ }
   eof             { Token.EOF }

%%

{- 
   Note: newline tokens in the grammar:
   It seems there are some dubious uses of NEWLINE in the grammar. 
   This is corroborated by this posting:
   http://mail.python.org/pipermail/python-dev/2005-October/057014.html
   The general idea is that the lexer does not generate NEWLINE tokens for
   lines which contain only spaces or comments. However, the grammar sometimes
   suggests that such tokens may exist. 
-}


-- single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE 

{- 
   Complete: but we don't support the newline at the end of a compound stmt 
   because the lexer would not produce a newline there. It seems like a weirdness
   in the way the interactive input works. 
-}

SingleInput :: { [Statement] }
SingleInput 
   : newline { [] }
   | SimpleStmt { $1 } 
   | CompoundStmt {- No newline here! -} { [$1] } 

-- file_input: (NEWLINE | stmt)* ENDMARKER

-- Complete: there is no need to mention the ENDMARKER, happy takes care of that.

FileInput :: { Program }
FileInput : ManyStmtOrNewline {- No ENDMARKER here! -} { Program $1 }

ManyStmtOrNewline :: { [Statement] }
ManyStmtOrNewline : ManyStmtOrNewlineRec { concat (reverse $1) }

ManyStmtOrNewlineRec :: { [[Statement]] }
ManyStmtOrNewlineRec 
   : {- empty -} { [] } 
   | ManyStmtOrNewlineRec NewLineOrStmt { $2 : $1 }

NewLineOrStmt :: { [Statement] }
NewLineOrStmt 
   : newline { [] }
   | Stmt    { $1 }

-- eval_input: testlist NEWLINE* ENDMARKER

-- Complete.

EvalInput :: { Expr }
EvalInput : TestList ZeroOrMoreNewline { $1 }

ZeroOrMoreNewline :: { () }
ZeroOrMoreNewline 
   : {- empty -} { () }
   | ZeroOrMoreNewline newline { () }
 
{-
  decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
  decorators: decorator+
  decorated: decorators (classdef | funcdef)
-}

-- Complete

Decorator :: { Decorator }
Decorator 
   : '@' DottedName OptionalArgList newline { Decorator { decorator_name = $2, decorator_args = $3 } }

Decorators :: { [Decorator] }
Decorators : DecoratorsRev { reverse $1 }

DecoratorsRev :: { [Decorator] }
DecoratorsRev 
   : Decorator { [$1] }
   | DecoratorsRev Decorator { $2 : $1 }

Decorated :: { Statement }
Decorated 
   : Decorators ClassOrFunction { Decorated { decorated_decorators = $1, decorated_def = $2 } } 

ClassOrFunction :: { Statement }
ClassOrFunction 
   : ClassDef { $1 }
   | FuncDef { $1 }

-- funcdef: 'def' NAME parameters ['->' test] ':' suite 

-- Complete

FuncDef :: { Statement }
FuncDef 
   : def Name Parameters OptionalResultAnnotation ':' Suite
     { Fun { fun_name = $2 , fun_args = $3, fun_result_annotation = $4, fun_body = $6 } }

OptionalResultAnnotation :: { Maybe Expr }
OptionalResultAnnotation
   : {- empty -} { Nothing }
   | '->' Test { Just $2 }

-- parameters: '(' [typedargslist] ')'

Parameters :: { [Parameter] }
Parameters : '(' TypedArgsList ')' { $2 }

{- 
   typedargslist: ((tfpdef ['=' test] ',')*
       ('*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
       | tfpdef ['=' test] (',' tfpdef ['=' test])* [',']) 
-} 

-- Complete. 

{- Note the grammar allows an optional trailing comma, but only after the
   positional arguments. If varargs are used (the star forms) then the
   optional comma is not allowed. Why is this so? I don't know.

   The code below uses right recursion extensively. The Happy docs say that
   there can be problems with this:

   http://www.haskell.org/happy/doc/html/sec-sequences.html#sec-separators

   "right-recursive rules require stack space proportional to the length 
   of the list being parsed. This can be extremely important where long sequences 
   are involved, for instance in automatically generated output."

   At the moment it seems easier to write using right recursion, but
   we may want to re-visit the use of right recursion at some point.
-} 

TypedArgsList :: { [Parameter] }
TypedArgsList : Params { $1 }

Params :: { [Parameter] }
Params
   : {- empty -} { [] }
   | Star { [$1] }
   | StarStar { [$1] }
   | Param { [$1] }
   | Param ',' Params { $1 : $3 }
   | Star ',' StarParams { $1 : $3 }

StarParams :: { [Parameter] }
StarParams 
   : Param { [$1] }
   | Param ',' StarParams { $1 : $3 }
   | StarStar { [$1] }
  
-- tfpdef: NAME [':' test]
-- Complete

TfpDef :: { (Ident, Maybe Expr) }
TfpDef : Name OptionalColonTest { ($1, $2) }

OptionalColonTest :: { Maybe Expr }
OptionalColonTest 
   : {- empty -} { Nothing }
   | ':' Test { Just $2 }

OptionalDefault :: { Maybe Expr }
OptionalDefault 
   : {- empty -} { Nothing }
   | '=' Test { Just $2 }

Param :: { Parameter }
Param 
   : TfpDef OptionalDefault { makeParam $1 $2 }

Star :: { Parameter }
Star : '*' OptionalTfpDef { makeStarParam $2 }

OptionalTfpDef :: { Maybe (Ident, Maybe Expr) }
OptionalTfpDef 
   : {- empty -} { Nothing }
   | TfpDef { Just $1 }

StarStar :: { Parameter }
StarStar : '**' TfpDef { makeStarStarParam $2 }

{- 
   varargslist: ((vfpdef ['=' test] ',')* ('*' [vfpdef] (',' vfpdef ['=' test])*  [',' '**' vfpdef] | '**' vfpdef) | vfpdef ['=' test] (',' vfpdef ['=' test])* [','])  

   vfpdef: NAME
-}

-- Complete

{- 
   There is some tedious similarity in these rules to the ones for
   TypedArgsList. VarArgsList is used for lambda functions, and they
   do not have parentheses around them (unlike function definitions).
   Therefore lambda parameters cannot have the optional annotations
   that normal functions can, because the annotations are introduced
   using a colon. This would cause ambibguity with the colon
   that marks the end of the lambda parameter list!

   See the remarks about right recursion in the comments for
   TypedArgsList.
-}

VarArgsList :: { [Parameter] }
VarArgsList : VParams { $1 }

VParams :: { [Parameter] }
VParams
   : {- empty -} { [] }
   | VStar { [$1] }
   | VStarStar { [$1] }
   | VParam { [$1] }
   | VParam ',' VParams { $1 : $3 }
   | VStar ',' VStarParams { $1 : $3 }

VStarParams :: { [Parameter] }
VStarParams 
   : VParam { [$1] }
   | VParam ',' VStarParams { $1 : $3 }
   | VStarStar { [$1] }

VParam :: { Parameter }
VParam : VfpDef OptionalDefault { makeParam ($1, Nothing) $2 }

VStar :: { Parameter }
VStar : '*' OptionalVfpDef { makeStarParam $2 }

OptionalVfpDef :: { Maybe (Ident, Maybe Expr) }
OptionalVfpDef 
   : {- empty -} { Nothing }
   | VfpDef { Just ($1, Nothing) }

VStarStar :: { Parameter }
VStarStar : '**' VfpDef { makeStarStarParam ($2, Nothing) }

VfpDef :: { Ident }
VfpDef : ident { Ident $1 }

Name :: { Ident }
Name : ident { Ident $1 }

-- stmt: simple_stmt | compound_stmt 

-- Complete

Stmt :: { [Statement] }
Stmt 
   : SimpleStmt { $1 }
   | CompoundStmt { [$1] } 

-- simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE 

-- Complete

SimpleStmt :: { [Statement] }
SimpleStmt : SmallStmts OptionalSemiColon newline { reverse $1 }

OptionalSemiColon :: { () }
OptionalSemiColon 
   : {- empty -} { () }
   | ';' { () }

SmallStmts :: { [Statement] }
SmallStmts : SmallStmt              { [$1] }
           | SmallStmts ';' SmallStmt { $3 : $1 }

{-
small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
             import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
-}

-- Complete

SmallStmt :: { Statement }
SmallStmt 
   : ExprStmt              { $1 }
   | DelStmt               { $1 }
   | PassStmt              { $1 }
   | FlowStmt              { $1 }
   | ImportStmt            { $1 }
   | GlobalStmt            { $1 }
   | NonLocalStmt          { $1 }
   | AssertStmt            { $1 }

-- expr_stmt: testlist (augassign (yield_expr|testlist) | ('=' (yield_expr|testlist))*)

-- Complete

ExprStmt :: { Statement }
ExprStmt : TestList Assignment { makeAssignmentOrExpr $1 $2 }

Assignment :: { Either [Expr] (AssignOp, Expr) }
Assignment 
   : NormalAssign { Left $1 }
   | AugAssign { Right $1 }

NormalAssign :: { [Expr] }
NormalAssign : ZeroOrMoreAssignRev { reverse $1 }

ZeroOrMoreAssignRev :: { [Expr] }
ZeroOrMoreAssignRev 
   : {- empty -} { [] }
   | ZeroOrMoreAssignRev '=' YieldOrTestList { $3 : $1 }

YieldOrTestList :: { Expr }
YieldOrTestList 
   : YieldExpr { $1 }
   | TestList { $1 }

{- 
   augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
            '<<=' | '>>=' | '**=' | '//=') 
-}

-- Complete

AugAssign :: { (AssignOp, Expr) }
AugAssign : AugAssignOp YieldOrTestList { ($1, $2) }

AugAssignOp :: { AssignOp }
AugAssignOp 
   : '+=' { AST.PlusAssign }
   | '-=' { AST.MinusAssign } 
   | '*=' { AST.MultAssign }
   | '/=' { AST.DivAssign }
   | '%=' { AST.ModAssign } 
   | '**=' { AST.PowAssign }
   | '&=' { AST.BinAndAssign } 
   | '|=' { AST.BinOrAssign }
   | '^=' { AST.BinXorAssign }
   | '<<=' { AST.LeftShiftAssign }
   | '>>=' { AST.RightShiftAssign }
   | '//=' { AST.FloorDivAssign } 

-- del_stmt: 'del' exprlist
-- Complete

DelStmt :: { Statement }
DelStmt : del ExprList { AST.Delete { del_exprs = $2 } }

-- pass_stmt: 'pass'
-- Complete

PassStmt :: { Statement }
PassStmt : pass { AST.Pass }

-- flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
-- Complete

FlowStmt :: { Statement }
FlowStmt 
   : BreakStmt { $1 }
   | ContinueStmt { $1 }
   | ReturnStmt { $1 }
   | RaiseStmt { $1 }
   | YieldStmt { $1 }

-- break_stmt: 'break'
-- Complete

BreakStmt :: { Statement }
BreakStmt : break { AST.Break }  

-- continue_stmt: 'continue'
-- Complete

ContinueStmt :: { Statement }
ContinueStmt : continue { AST.Continue }  

-- return_stmt: 'return' [testlist]
-- Complete 

ReturnStmt :: { Statement }
ReturnStmt : return OptionalTestList { AST.Return { return_expr = $2 }}

-- yield_stmt: yield_expr
-- Complete

YieldStmt :: { Statement }
YieldStmt : YieldExpr { StmtExpr { stmt_expr = $1 } } 

-- raise_stmt: 'raise' [test ['from' test]]
-- Complete 

RaiseStmt :: { Statement }
RaiseStmt : raise OptionalTestFrom { AST.Raise { raise_expr = $2 }}

OptionalTestFrom :: { Maybe (Expr, Maybe Expr) }
OptionalTestFrom 
   : {- empty -} { Nothing }
   | Test OptionalFrom { Just ($1, $2) }

OptionalFrom :: { Maybe Expr }
OptionalFrom 
   : {- empty -} { Nothing }
   | from Test { Just $2 }

-- import_stmt: import_name | import_from
-- Complete

ImportStmt :: { Statement }
ImportStmt 
   : ImportName { $1 }
   | ImportFrom { $1 }

{-
   # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
   import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
                 'import' ('*' | '(' import_as_names ')' | import_as_names))
-}

ImportFrom :: { Statement }
ImportFrom : from ImportModule import StarOrAsNames { FromImport { from_module = $2, from_items = $4 }}

StarOrAsNames :: { FromItems }
StarOrAsNames
   : '*' { ImportEverything }
   | '(' ImportAsNames ')' { $2 }
   | ImportAsNames { $1 } 

ImportModule :: { ImportModule }
ImportModule 
   : '.' { ImportDot }
   | '...' { ImportRelative (ImportRelative ImportDot) }
   | DottedName { ImportName $1 }
   | '.' ImportModule { ImportRelative $2 }
   | '...' ImportModule { ImportRelative (ImportRelative (ImportRelative $2)) }

-- import_as_name: NAME ['as' NAME]
ImportAsName :: { FromItem }
ImportAsName 
   : Name OptionalAsName { FromItem { from_item_name = $1, from_as_name = $2 }}

-- import_as_names: import_as_name (',' import_as_name)* [',']
ImportAsNames :: { FromItems }
ImportAsNames : ImportAsNamesRev OptionalComma { FromItems (reverse $1) }

ImportAsNamesRev :: { [FromItem] }
ImportAsNamesRev
   : ImportAsName { [$1] }
   | ImportAsNamesRev ',' ImportAsName { $3 : $1 }

-- import_name: 'import' dotted_as_names

ImportName :: { Statement }
ImportName : import DottedAsNames { AST.Import { import_items = $2 }}

-- dotted_as_names: dotted_as_name (',' dotted_as_name)*

DottedAsNames :: { [ImportItem] }
DottedAsNames : OneOrMoreDottedAsNamesRev { reverse $1 }

OneOrMoreDottedAsNamesRev :: { [ImportItem] }
OneOrMoreDottedAsNamesRev
   : DottedAsName { [$1] }
   | OneOrMoreDottedAsNamesRev ',' DottedAsName { $3 : $1 }

-- dotted_as_name: dotted_name ['as' NAME]

DottedAsName :: { ImportItem }
DottedAsName 
   : DottedName OptionalAsName  
        { ImportItem { import_item_name = $1, import_as_name = $2 }}

-- dotted_name: NAME ('.' NAME)* 
-- Complete

DottedName :: { DottedName }
DottedName : Name DotNames { $1 : reverse $2 }

DotNames :: { DottedName }
DotNames 
   : {- empty -} { [] }
   | DotNames '.' Name { $3 : $1 }

-- global_stmt: 'global' NAME (',' NAME)*
-- Complete

GlobalStmt :: { Statement }
GlobalStmt : global OneOrMoreNames { AST.Global { global_vars = $2 }}

OneOrMoreNames :: { [Ident] }
OneOrMoreNames : OneOrMoreNamesRev { reverse $1 }

OneOrMoreNamesRev :: { [Ident] }
OneOrMoreNamesRev
   : Name { [$1] }
   | OneOrMoreNamesRev ',' Name { $3 : $1 }

-- nonlocal_stmt: 'nonlocal' NAME (',' NAME)*

NonLocalStmt :: { Statement }
NonLocalStmt : nonlocal OneOrMoreNames { AST.NonLocal { nonLocal_vars = $2 }}

-- assert_stmt: 'assert' test [',' test]

AssertStmt :: { Statement }
AssertStmt : assert TestListRev { AST.Assert { assert_exprs = reverse $2 }}

-- compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated 
-- Complete

CompoundStmt :: { Statement }
CompoundStmt 
   : IfStmt { $1 } 
   | WhileStmt { $1 }
   | ForStmt { $1 }
   | TryStmt { $1 }
   | WithStmt { $1 }
   | FuncDef { $1 } 
   | ClassDef { $1 }
   | Decorated { $1 }

-- if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
-- Complete

IfStmt :: { Statement }
IfStmt : IfConditionals OptionalElse { Conditional { cond_guards = $1, cond_else = $2 } }

IfConditionals :: { [(Expr,[Statement])] }
IfConditionals : If ZeroOrMoreElifs { $1 : $2 }

If :: { (Expr, [Statement]) }
If : if Test ':' Suite { ($2, $4) }

ZeroOrMoreElifs :: { [(Expr, [Statement])]}
ZeroOrMoreElifs : ZeroOrMoreElifsRev { reverse $1 }

ZeroOrMoreElifsRev :: { [(Expr, [Statement])]}
ZeroOrMoreElifsRev 
   : {- empty -} { [] }
   | ZeroOrMoreElifsRev elif Test ':' Suite { ($3, $5) : $1 }

OptionalElse :: { [Statement] }
OptionalElse 
   : {- empty -} { [] }
   | else ':' Suite { $3 }

-- while_stmt: 'while' test ':' suite ['else' ':' suite] 
-- Complete

WhileStmt :: { Statement }
WhileStmt : while Test ':' Suite OptionalElse { AST.While { while_cond = $2 , while_body = $4, while_else = $5 } }

-- for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite] 
-- Complete

ForStmt :: { Statement }
ForStmt 
   : for ExprList in TestList ':' Suite OptionalElse 
     { AST.For { for_targets = $2, for_generator = $4, for_body = $6, for_else = $7 } }

{- 
   try_stmt: ('try' ':' suite 
               ((except_clause ':' suite)+ ['else' ':' suite] ['finally' ':' suite] | 'finally' ':' suite))
-}
-- Complete

TryStmt :: { Statement }
TryStmt : try ':' Suite Handlers { makeTry $3 $4 }

Handlers :: { ([Handler], [Statement], [Statement]) }
Handlers 
   : OneOrMoreExceptClauses OptionalElse OptionalFinally { ($1, $2, $3) }
   | finally ':' Suite { ([], [], $3) }

OptionalFinally :: { [Statement] }
OptionalFinally 
   : {- empty -} { [] }
   | finally ':' Suite { $3 }

OneOrMoreExceptClauses :: { [Handler] }
OneOrMoreExceptClauses : OneOrMoreExceptClausesRev { reverse $1 }

OneOrMoreExceptClausesRev :: { [Handler] }
OneOrMoreExceptClausesRev 
   : Handler { [$1] }
   | OneOrMoreExceptClausesRev Handler { $2 : $1 }

Handler :: { Handler }
Handler : ExceptClause ':' Suite { ($1, $3) }

{- 
   with_stmt: 'with' test [ with_var ] ':' suite
   with_var: 'as' expr
-}
-- Complete

WithStmt :: { Statement }
WithStmt : with Test OptionalAs ':' Suite 
           { AST.With { with_context = $2, with_as = $3, with_body = $5 } }

OptionalAs :: { Maybe Expr }
OptionalAs 
   : {- empty -} { Nothing }
   | as Expr { Just $2 }

-- except_clause: 'except' [test ['as' NAME]] 
-- Complete

ExceptClause :: { ExceptClause }
ExceptClause : except ExceptExpr { $2 }

ExceptExpr :: { ExceptClause }
ExceptExpr 
   : {- empty -} { Nothing }
   | Test OptionalAsName { Just ($1, $2) }

OptionalAsName :: { Maybe Ident }
OptionalAsName 
   : {- empty -} { Nothing }
   | as Name     { Just $2 }

-- suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT 
-- Complete, but we don't have a newline before indent b/c it is redundant

Suite :: { [Statement] }
Suite 
   : SimpleStmt { $1 }
   | {- no newline here! -} indent OneOrMoreStmts dedent { $2 } 

OneOrMoreStmts :: { [Statement] }
OneOrMoreStmts : OneOrMoreStmtsRec { reverse (concat $1) }

OneOrMoreStmtsRec :: { [[Statement]] }
OneOrMoreStmtsRec 
   : Stmt { [$1] }
   | OneOrMoreStmtsRec Stmt { $2 : $1 }

-- test: or_test ['if' or_test 'else' test] | lambdef
-- Complete

Test :: { Expr }
Test 
   : OrTest TestCond { makeConditionalExpr $1 $2 }
   | LambDef { $1 }

TestCond :: { Maybe (Expr, Expr) }
TestCond 
   : {- empty -} { Nothing }
   | if OrTest else Test { Just ($2, $4) }

-- test_nocond: or_test | lambdef_nocond
-- Complete 

TestNoCond :: { Expr }
TestNoCond
   : OrTest { $1 }
   | LambDefNoCond { $1 }

-- lambdef: 'lambda' [varargslist] ':' test
-- Complete

LambDef :: { Expr }
LambDef : lambda VarArgsList ':' Test { AST.Lambda $2 $4 }

-- lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
-- Complete

LambDefNoCond :: { Expr }
LambDefNoCond : lambda VarArgsList ':' TestNoCond { AST.Lambda $2 $4 }

-- or_test: and_test ('or' and_test)* 
-- Complete

OrTest :: { Expr }
OrTest : AndTest OrSequence { makeBinOp $1 $2 }

OrSequence :: { [(Op, Expr)] }
OrSequence 
   : {- empty -} { [] }
   | OrSequence or AndTest { (AST.Or, $3) : $1 }

-- and_test: not_test ('and' not_test)* 
-- Complete 

AndTest :: { Expr }
AndTest : NotTest AndSequence { makeBinOp $1 $2 }

AndSequence :: { [(Op, Expr)] }
AndSequence 
   : {- empty -} { [] }
   | AndSequence and NotTest { (AST.And, $3) : $1 }

-- not_test: 'not' not_test | comparison 
-- Complete

NotTest :: { Expr }
NotTest 
   : not NotTest { UnaryOp {operator = AST.Not, op_arg = $2} }
   | Comparison { $1 }

-- comparison: star_expr (comp_op star_expr)*
-- Complete

Comparison :: { Expr }
Comparison : StarExpr CompSequence { makeBinOp $1 $2 }

CompSequence :: { [(Op, Expr)] }
CompSequence 
   : {- empty -} { [] }
   | CompSequence CompOp StarExpr { ($2, $3) : $1 }

-- comp_op: '<'|'>'|'=='|'>='|'<='|'!='|'in'|'not' 'in'|'is'|'is' 'not' 
-- Complete

CompOp :: { Op }
CompOp 
   : '<'    { AST.LessThan }
   | '>'    { AST.GreaterThan }
   | '=='   { AST.Equality }
   | '>='   { AST.GreaterThanEquals }
   | '<='   { AST.LessThanEquals }
   | '!='   { AST.NotEquals }
   | in     { AST.In }
   | not in { AST.NotIn }
   | IsOp   { $1 }

IsOp :: { Op }
IsOp : is NotPart { $2 }

NotPart :: { Op }
NotPart 
   : {- empty -} { AST.Is }
   | not { AST.IsNot } 

-- star_expr: ['*'] expr 
-- Incomplete
{- 
   XXX The grammar grossly over-states the places where a starred expression can occur.
   It leads to an ambiguity because of the starred argument lists. 

   I think this is a bug in the grammar, and it will need more investigation to see if
   it can be fixed.
-}

StarExpr :: { Expr }
StarExpr : Expr { $1 }
{-
   : '*' Expr { Starred { starred_expr = $2 }} 
   | Expr { $1 }
-}

-- expr: xor_expr ('|' xor_expr)* 
-- Complete

Expr :: { Expr }
Expr : XorExpr BinaryOrSequence { makeBinOp $1 $2 }

BinaryOrSequence :: { [(Op, Expr)] }
BinaryOrSequence 
   : {- empty -} { [] }
   | BinaryOrSequence '|' XorExpr { (AST.BinaryOr, $3) : $1 }

-- xor_expr: and_expr ('^' and_expr)* 
-- Complete

XorExpr :: { Expr }
XorExpr : AndExpr XorSequence { makeBinOp $1 $2 }

XorSequence :: { [(Op, Expr)] }
XorSequence 
   : {- empty -} { [] }
   | XorSequence '^' AndExpr { (AST.Xor, $3) : $1 }

-- and_expr: shift_expr ('&' shift_expr)* 
-- Complete

AndExpr :: { Expr }
AndExpr : ShiftExpr BinaryAndSequence { makeBinOp $1 $2 }

BinaryAndSequence :: { [(Op, Expr)] }
BinaryAndSequence 
   : {- empty -} { [] }
   | BinaryAndSequence '&' ShiftExpr { (AST.BinaryAnd, $3) : $1 }

-- shift_expr: arith_expr (('<<'|'>>') arith_expr)* 
-- Complete

ShiftExpr :: { Expr }
ShiftExpr : ArithExpr ShiftSequence { makeBinOp $1 $2 }

ShiftSequence :: { [(Op, Expr)] }
ShiftSequence 
   : {- empty -} { [] }
   | ShiftSequence ShiftOp ArithExpr { ($2, $3) : $1 }

ShiftOp :: { Op }
ShiftOp 
   : '<<' { AST.ShiftLeft }
   | '>>' { AST.ShiftRight }

-- arith_expr: term (('+'|'-') term)*
-- Complete

ArithExpr :: { Expr }
ArithExpr : Term TermSequence { makeBinOp $1 $2 }

TermSequence :: { [(Op, Expr)] }
TermSequence 
   : {- empty -} { [] }
   | TermSequence ArithOp Term { ($2, $3) : $1 }

ArithOp :: { Op }
ArithOp 
   : '+' { AST.Plus }
   | '-' { AST.Minus }

-- term: factor (('*'|'/'|'%'|'//') factor)* 
-- Complete

Term :: { Expr }
Term : Factor FactorSequence { makeBinOp $1 $2 }

FactorSequence :: { [(Op, Expr)] }
FactorSequence 
   : {- empty -} { [] }
   | FactorSequence MultDivOp Factor { ($2, $3) : $1 }

MultDivOp :: { Op }
MultDivOP 
   : '*' { AST.Multiply } 
   | '/' { AST.Divide }
   | '%' { AST.Modulo }
   | floordiv { AST.FloorDivide }

-- factor: ('+'|'-'|'~') factor | power 
-- Complete

Factor :: { Expr }
Factor 
   : '+' Factor { UnaryOp { operator = AST.Plus, op_arg = $2 } } 
   | '-' Factor { UnaryOp { operator = AST.Minus, op_arg = $2 } } 
   | '~' Factor { UnaryOp { operator = AST.Invert, op_arg = $2 } } 
   | Power { $1 }

-- power: atom trailer* ['**' factor]
-- Complete, but maybe we should factor out the common prefix?

Power :: { Expr }
Power : Atom ZeroOrMoreTrailer { addTrailer $1 $2 }
      | Atom ZeroOrMoreTrailer '**' Factor 
        { makeBinOp (addTrailer $1 $2) [(AST.Exponent, $4)] } 

ZeroOrMoreTrailer :: { [Trailer] }
ZeroOrMoreTrailer : ZeroOrMoreTrailerRev { reverse $1 }

ZeroOrMoreTrailerRev :: { [Trailer] }
ZeroOrMoreTrailerRev 
   : {- empty -} { [] }
   | ZeroOrMoreTrailerRev Trailer { $2 : $1 }

{- 
   atom: ('(' [yield_expr|testlist_comp] ')' |
          '[' [testlist_comp] ']' |
          '{' [dictorsetmaker] '}' |
           NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
-}
-- Incomplete

Atom :: { Expr }
Atom : ParenForm { $1 } 
     | ListForm { $1 }
     | DictOrSetForm { $1 }
     | Name { AST.Var $1 }
     | integer { AST.Int $1 }
     | float { AST.Float $1 }
     | imaginary { AST.Imaginary { imaginary_value = $1 }}
     | OneOrMoreStrings { AST.Strings (reverse $1) }
     | OneOrMoreByteStrings { AST.ByteStrings (reverse $1) }
     | '...' { AST.Ellipsis }
     | none { AST.None }
     | true { AST.Bool Prelude.True }
     | false { AST.Bool Prelude.False }

ParenForm :: { Expr }
ParenForm : '(' YieldOrTestListComp ')' { $2 }

ListForm :: { Expr }
ListForm 
   : '[' ']' { List { list_exprs = [] } }
   | '[' TestListComp ']' { makeListForm $2 }

DictOrSetForm :: { Expr }
DictOrSetForm
   : '{' '}' { Dictionary { dict_mappings = [] }}
   | '{' DictOrSetMaker '}' { $2 }

YieldOrTestListComp :: { Expr }
YieldOrTestListComp 
   : {- empty -} { Tuple { tuple_exprs = [] } }
   | YieldExpr { $1 }
   | TestListComp { either id (\c -> Generator { gen_comprehension = c }) $1 } 

OneOrMoreStrings :: { [String] }
OneOrMoreStrings
   : string { [$1] }
   | OneOrMoreStrings string { $2 : $1 }

OneOrMoreByteStrings :: { [BS.ByteString] }
OneOrMoreByteStrings
   : bytestring { [$1] }
   | OneOrMoreByteStrings bytestring { $2 : $1 }

-- testlist_comp: test ( comp_for | (',' test)* [','] )
-- Complete 

TestListComp :: { Either Expr (Comprehension Expr) }
TestListComp
   : TestList { Left $1 }
   | Test CompFor { Right (makeComprehension $1 $2) }

-- trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME 
-- Complete 

Trailer :: { Trailer }
Trailer 
   : '(' ArgList ')' { TrailerCall $2 }
   | '[' SubscriptList ']' { TrailerSubscript $2 } 
   | '.' Name { TrailerDot $2 }

-- subscriptlist: subscript (',' subscript)* [',']

SubscriptList :: { [Subscript] }
SubscriptList : OneOrMoreSubsRev OptionalComma { reverse $1 }

OneOrMoreSubsRev :: { [Subscript] }
OneOrMoreSubsRev
   : Subscript { [$1] }
   | OneOrMoreSubsRev ',' Subscript { $3 : $1 }

-- subscript: test | [test] ':' [test] [sliceop]

Subscript :: { Subscript }
Subscript
   : Test { SubscriptExpr $1 }
   | OptionalTest ':' OptionalTest OptionalSliceOp { SubscriptSlice $1 $3 $4 }

OptionalTest :: { Maybe Expr }
OptionalTest
   : {- empty -} { Nothing }
   | Test { Just $1 }

OptionalSliceOp :: { Maybe (Maybe Expr) }
OptionalSliceOp 
   : {- empty -} { Nothing }
   | SliceOp { Just $1 }

-- sliceop: ':' [test]

SliceOp :: { Maybe Expr }
SliceOp : ':' OptionalTest { $2 }

-- exprlist: star_expr (',' star_expr)* [',']
-- Complete

ExprList :: { [Expr] }
ExprList : ExprListRev OptionalComma { reverse $1 }
       
OptionalComma :: { Bool }
OptionalComma 
   : {- empty -} { False }
   | ',' { True }  

ExprListRev :: { [Expr] }
ExprListRev 
   : StarExpr { [$1] }
   | ExprListRev ',' StarExpr { $3 : $1 }

-- testlist: test (',' test)* [',']
-- Complete

-- Some trickery here because the of the optional trailing comma, which
-- could turn a normal expression into a tuple.
-- Very occasionally, TestList is used to generate something which is not
-- a tuple (such as the square bracket notation in list literals). Therefore
-- it would seem like a good idea to not return a tuple in this case, but
-- a list of expressions. However this would complicate a lot of code
-- since we would have to carry around the optional comma information.
-- I've decided to leave it as a tuple, and in special cases, unpack the
-- tuple and pull out the list of expressions.

TestList :: { Expr }
TestList : TestListRev OptionalComma { makeTupleOrExpr (reverse $1) $2 }
       
TestListRev :: { [Expr] }
TestListRev 
   : Test { [$1] }
   | TestListRev ',' Test { $3 : $1 }

{- 
   dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
                   (test (comp_for | (',' test)* [','])) )
-}

DictOrSetMaker :: { Expr }
DictOrSetMaker
   : Test ':' Test DictRest { makeDictionary ($1, $3) $4 } 
   | Test SetRest { makeSet $1 $2 } 

DictRest :: { Either CompFor [(Expr, Expr)] }
DictRest
   : CompFor { Left $1 }
   | ZeroOrMoreDictMappings OptionalComma { Right (reverse $1) }

ZeroOrMoreDictMappings :: { [(Expr, Expr)] }
ZeroOrMoreDictMappings
   : {- empty -} { [] }
   | ZeroOrMoreDictMappings ',' Test ':' Test { ($3,$5) : $1 }

SetRest :: { Either CompFor [Expr] }
SetRest
   : CompFor { Left $1 }
   | ZeroOrMoreCommaTest OptionalComma { Right (reverse $1) }

ZeroOrMoreCommaTest :: { [Expr] }
ZeroOrMoreCommaTest 
   : {- empty -} { [] }
   | ZeroOrMoreCommaTest ',' Test { $3 : $1 }

-- classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- Complete

ClassDef :: { Statement }
ClassDef 
   : class Name OptionalArgList ':' Suite 
     { AST.Class { class_name = $2, class_args = $3, class_body = $5 }}

OptionalArgList :: { [Argument] }
OptionalArgList 
   : {- empty -} { [] }
   | '(' ArgList ')' { $2 }

{- 
   arglist: (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test]
                         |'**' test)
-}

{-
   Deviates slightly from the grammar because we allow empty arg lists.
   The grammar allows for this by making arg lists non-empty but optional.
   Works out the same in the end.
-}

ArgList :: { [Argument] }
ArgList 
   : {- empty -} { [] }
   | '*' Test { [ArgVarArgsPos { arg_expr = $2 }] }
   | '**' Test { [ArgVarArgsKeyword { arg_expr = $2 }] }
   | Argument { [$1] }
   | Argument ',' ArgList { $1 : $3 }
   | '*' Test ',' StarArgs { ArgVarArgsPos { arg_expr = $2 } : $4 }

StarArgs :: { [Argument] }
StarArgs
   : Argument { [$1] }
   | Argument ',' StarArgs { $1 : $3 }
   | '**' Test { [ArgVarArgsKeyword { arg_expr = $2 }] }

-- argument: test [comp_for] | test '=' test  # Really [keyword '='] test

Argument :: { Argument }
Argument
   : Name '=' Test { ArgKeyword { arg_keyword = $1, arg_expr = $3 }}
   | Test { ArgExpr { arg_expr = $1 }} 
   | Test CompFor { ArgExpr { arg_expr = Generator { gen_comprehension = makeComprehension $1 $2 }}}

-- comp_iter: comp_for | comp_if
-- Complete

CompIter :: { CompIter }
CompIter
   : CompFor { IterFor $1 }
   | CompIf { IterIf $1 } 

-- comp_for: 'for' exprlist 'in' or_test [comp_iter]

CompFor :: { CompFor }
CompFor : for ExprList in OrTest OptionalCompIter 
          { CompFor { comp_for_exprs = $2, comp_in_expr = $4, comp_for_iter = $5 }}

OptionalCompIter :: { Maybe CompIter }
OptionalCompIter
   : {- empty -} { Nothing }
   | CompIter { Just $1 }

-- comp_if: 'if' test_nocond [comp_iter]

CompIf :: { CompIf }
CompIf : if TestNoCond OptionalCompIter { CompIf { comp_if = $2, comp_if_iter = $3 } }

-- testlist1: test (',' test)*
-- Not used in the rest of the grammar!

-- encoding_decl: NAME
-- Not used in the rest of the grammqr!

-- yield_expr: 'yield' [testlist] 
-- Complete

YieldExpr :: { Expr }
YieldExpr : yield OptionalTestList { AST.Yield { yield_expr = $2 } }

OptionalTestList :: { Maybe Expr }
OptionalTestList 
   : {- empty -} { Nothing }
   | TestList    { Just $1 }

{
-- Put additional Haskell code in here if needed.

}
