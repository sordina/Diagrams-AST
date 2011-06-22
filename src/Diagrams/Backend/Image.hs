{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeSynonymInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Image
-- Copyright   :  (c) 2011 Lyndon Maydwell (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  maydwell@gmail.com
--
-- An Image-AST diagrams backend
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Image where

import Diagrams.Prelude

import Diagrams.TwoD.Ellipse

import Data.Basis

import Text.PrettyPrint (Doc, empty, ($+$), parens, hsep, text, nest)
import qualified Text.PrettyPrint as PP

import Data.List (transpose)

-- | Token for identifying this backend.
data ImageBackend = ImageBackend

instance HasLinearMap v => Backend ImageBackend v where
  data Render  ImageBackend v = SR Doc
  type Result  ImageBackend v = String
  data Options ImageBackend v = SBOpt

  withStyle _ _ _ r = r -- XXX FIXME

  doRender _ _ (SR r) = PP.render r

instance Monoid (Render ImageBackend v) where
  mempty = SR empty
  (SR d1) `mappend` (SR d2) = SR (d1 $+$ d2)

renderTransf :: forall v. (Num (Scalar v), HasLinearMap v)
             => Transformation v -> Doc
renderTransf t = renderMat mat
  where tr :: v
        tr    = transl t
        basis :: [Basis v]
        basis = map fst (decompose tr)
        es :: [v]
        es    = map basisValue basis
        vmat :: [v]
        vmat = map (apply t) es
        mat :: [[Scalar v]]
        mat = map decompV vmat
--        mat' :: [[Scalar v]]
--        mat'  = map (++[0]) mat ++ [decompV tr ++ [1]]
        decompV = map snd . decompose

renderMat :: Show a => [[a]] -> Doc
renderMat = PP.vcat . map renderRow . transpose
  where renderRow = parens . hsep . map (text . show)

instance Renderable Ellipse ImageBackend where
  render _ (Ellipse t) = SR $ text "Ellipse (" $+$
                                (nest 2 (renderTransf t)) $+$
                              text ")"

instance (Show v, HasLinearMap v) => Renderable (Segment v) ImageBackend where
  render _ s = SR $ text (show s)

instance (Show v, HasLinearMap v) => Renderable (Trail v) ImageBackend where
  render _ t = SR $ text (show t)

instance (Ord v, Show v, HasLinearMap v) => Renderable (Path v) ImageBackend where
  render _ p = SR $ text (show p)
