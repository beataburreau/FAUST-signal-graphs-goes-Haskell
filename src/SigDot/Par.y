-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module SigDot.Par
  ( happyError
  , myLexer
  , pDotGraph
  , pStmt
  , pListStmt
  , pAttr
  , pListAttr
  , pAKind
  , pKind
  , pGKind
  ) where

import Prelude

import qualified SigDot.Abs
import SigDot.Lex

}

%name pDotGraph DotGraph
%name pStmt Stmt
%name pListStmt ListStmt
%name pAttr Attr
%name pListAttr ListAttr
%name pAKind AKind
%name pKind Kind
%name pGKind GKind
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '->'        { PT _ (TS _ 1)  }
  ';'         { PT _ (TS _ 2)  }
  '='         { PT _ (TS _ 3)  }
  '['         { PT _ (TS _ 4)  }
  ']'         { PT _ (TS _ 5)  }
  'color'     { PT _ (TS _ 6)  }
  'digraph'   { PT _ (TS _ 7)  }
  'edge'      { PT _ (TS _ 8)  }
  'fillcolor' { PT _ (TS _ 9)  }
  'fontsize'  { PT _ (TS _ 10) }
  'graph'     { PT _ (TS _ 11) }
  'label'     { PT _ (TS _ 12) }
  'node'      { PT _ (TS _ 13) }
  'rankdir'   { PT _ (TS _ 14) }
  'shape'     { PT _ (TS _ 15) }
  'strict'    { PT _ (TS _ 16) }
  'style'     { PT _ (TS _ 17) }
  'subgraph'  { PT _ (TS _ 18) }
  '{'         { PT _ (TS _ 19) }
  '}'         { PT _ (TS _ 20) }
  L_quoted    { PT _ (TL $$)   }
  L_ID        { PT _ (T_ID $$) }

%%

String  :: { String }
String   : L_quoted { $1 }

ID :: { SigDot.Abs.ID }
ID  : L_ID { SigDot.Abs.ID $1 }

DotGraph :: { SigDot.Abs.DotGraph }
DotGraph
  : 'strict' GKind ID '{' ListStmt '}' { SigDot.Abs.GDef $2 $3 $5 }

Stmt :: { SigDot.Abs.Stmt }
Stmt
  : ID '[' ListAttr ']' { SigDot.Abs.SNode $1 $3 }
  | ID '->' ID '[' ListAttr ']' { SigDot.Abs.SEdge $1 $3 $5 }
  | Kind '[' ListAttr ']' { SigDot.Abs.SAttr $1 $3 }
  | Attr { SigDot.Abs.SSoloAttr $1 }

ListStmt :: { [SigDot.Abs.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ';' ListStmt { (:) $1 $3 }

Attr :: { SigDot.Abs.Attr }
Attr
  : AKind '=' String { SigDot.Abs.ARegular $1 $3 }
  | AKind '=' ID { SigDot.Abs.AID $1 $3 }

ListAttr :: { [SigDot.Abs.Attr] }
ListAttr : {- empty -} { [] } | Attr ListAttr { (:) $1 $2 }

AKind :: { SigDot.Abs.AKind }
AKind
  : 'fontsize' { SigDot.Abs.AFontsize }
  | 'rankdir' { SigDot.Abs.ARankdir }
  | 'label' { SigDot.Abs.ALabel }
  | 'color' { SigDot.Abs.AColor }
  | 'fillcolor' { SigDot.Abs.AFillcolor }
  | 'style' { SigDot.Abs.AStyle }
  | 'shape' { SigDot.Abs.AShape }

Kind :: { SigDot.Abs.Kind }
Kind
  : 'node' { SigDot.Abs.KNode }
  | 'edge' { SigDot.Abs.KEgde }
  | 'graph' { SigDot.Abs.KGraph }

GKind :: { SigDot.Abs.GKind }
GKind
  : 'digraph' { SigDot.Abs.GKDi } | 'subgraph' { SigDot.Abs.GKSub }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}
