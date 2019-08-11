{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Miller.Pretty
  ( showExpr
  , showProgram
  , renderShow
  , renderDoc
  ) where

import Doors
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty

import Miller.Expr

renderDoc :: Doc a -> String
renderDoc = Pretty.renderString . layoutSmart defaultLayoutOptions

renderShow :: Pretty a => a -> String
renderShow = renderDoc . pretty

showExpr :: CoreExpr -> String
showExpr = renderDoc . prettyExpr

showProgram :: CoreProgram -> String
showProgram = renderDoc . prettyProgram

prettyProgram :: CoreProgram -> Doc a
prettyProgram (Program ps) = vsep . toList $ fmap prettyDefn ps

prettyDefn :: CoreDefn -> Doc a
prettyDefn (Defn n params b) = pretty n <+> hsep (pretty <$> params) <+> "=" <+> prettyExpr b

-- Snatched from the Semantic definition. Correctly pretty-printing with precedence
-- is surprisingly, and sometimes depressingly, difficult.
data Prec a = Prec
  { precLevel :: Maybe Int
  , precBody  :: a
  } deriving (Eq, Ord, Show)

prec :: Int -> a -> Prec a
prec = Prec . Just

atom :: a -> Prec a
atom = Prec Nothing

withPrec :: Int -> Prec (Doc a) -> Doc a
withPrec level (Prec mCurr bod)
  | maybe False (level >) mCurr = parens bod
  | otherwise                   = bod

prettyRec :: Rec -> Doc a
prettyRec = \case
  Rec -> "letrec"
  Non -> "let"

prettyBindings :: NonEmpty (Name, CoreExpr) -> Doc a
prettyBindings = group . align . vcat . punctuate semi . toList . fmap go
  where go (l, r) = pretty l <+> "=" <+> prettyExpr r

prettyAlts :: NonEmpty (Int, [Name], CoreExpr) -> Doc a
prettyAlts = align . vcat . punctuate semi . toList . fmap go
  where go (tag, names, bod) = pretty tag <+> hsep (pretty <$> names) <+> "->" <+> prettyExpr bod

prettyExpr :: CoreExpr -> Doc a
prettyExpr = precBody . go
  where
    go :: CoreExpr -> Prec (Doc a)
    go = \case
      Var n  -> atom (pretty n)
      Num i  -> atom (pretty i)
      Ap l r -> prec 8 (withPrec 8 (go l) <+> withPrec 9 (go r))
      Constr t x -> atom (pretty t <> "#" <> pretty x)
      Let r bs body -> prec 3 (prettyRec r <+> prettyBindings bs <+> "in" <+> withPrec 0 (go body))
      Case v alts -> prec 3 ("case" <+> precBody (go v) <+> "of" <+> prettyAlts alts)
      Lam vs bod -> prec 3 ("λ" <> hsep (pretty <$> vs) <+> "->" <+> precBody (go bod))
      Binary Add l r -> prec 6 (withPrec 7 (go l) <+> "+" <+> withPrec 6 (go r))
      Binary Sub l r -> prec 6 (withPrec 7 (go l) <+> "-" <+> withPrec 6 (go r))
      Binary Mul l r -> prec 7 (withPrec 8 (go l) <+> "+" <+> withPrec 7 (go r))

