-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for SigDot.

module SigDot.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified SigDot.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print SigDot.Abs.ID where
  prt _ (SigDot.Abs.ID i) = doc $ showString i
instance Print SigDot.Abs.DotGraph where
  prt i = \case
    SigDot.Abs.GDef gkind id_ stmts -> prPrec i 0 (concatD [doc (showString "strict"), prt 0 gkind, prt 0 id_, doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print SigDot.Abs.Stmt where
  prt i = \case
    SigDot.Abs.SNode id_ attrs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 attrs, doc (showString "]")])
    SigDot.Abs.SEdge id_1 id_2 attrs -> prPrec i 0 (concatD [prt 0 id_1, doc (showString "->"), prt 0 id_2, doc (showString "["), prt 0 attrs, doc (showString "]")])
    SigDot.Abs.SAttr kind attrs -> prPrec i 0 (concatD [prt 0 kind, doc (showString "["), prt 0 attrs, doc (showString "]")])
    SigDot.Abs.SSoloAttr attr -> prPrec i 0 (concatD [prt 0 attr])

instance Print [SigDot.Abs.Stmt] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print SigDot.Abs.Attr where
  prt i = \case
    SigDot.Abs.ARegular akind str -> prPrec i 0 (concatD [prt 0 akind, doc (showString "="), printString str])
    SigDot.Abs.AID akind id_ -> prPrec i 0 (concatD [prt 0 akind, doc (showString "="), prt 0 id_])

instance Print [SigDot.Abs.Attr] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print SigDot.Abs.AKind where
  prt i = \case
    SigDot.Abs.AFontsize -> prPrec i 0 (concatD [doc (showString "fontsize")])
    SigDot.Abs.ARankdir -> prPrec i 0 (concatD [doc (showString "rankdir")])
    SigDot.Abs.ALabel -> prPrec i 0 (concatD [doc (showString "label")])
    SigDot.Abs.AColor -> prPrec i 0 (concatD [doc (showString "color")])
    SigDot.Abs.AFillcolor -> prPrec i 0 (concatD [doc (showString "fillcolor")])
    SigDot.Abs.AStyle -> prPrec i 0 (concatD [doc (showString "style")])
    SigDot.Abs.AShape -> prPrec i 0 (concatD [doc (showString "shape")])

instance Print SigDot.Abs.Kind where
  prt i = \case
    SigDot.Abs.KNode -> prPrec i 0 (concatD [doc (showString "node")])
    SigDot.Abs.KEgde -> prPrec i 0 (concatD [doc (showString "edge")])
    SigDot.Abs.KGraph -> prPrec i 0 (concatD [doc (showString "graph")])

instance Print SigDot.Abs.GKind where
  prt i = \case
    SigDot.Abs.GKDi -> prPrec i 0 (concatD [doc (showString "digraph")])
    SigDot.Abs.GKSub -> prPrec i 0 (concatD [doc (showString "subgraph")])