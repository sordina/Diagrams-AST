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
-- "Abstract diagrams are rendered to particular formats by backends. Each
-- backend/vector space combination must be an instance of the Backend class. A
-- minimal complete definition consists of the three associated types and
-- implementations for withStyle and doRender."

{-
class (HasLinearMap v, Monoid (Render b v)) => Backend b v where
  withStyle ::
    b -> Style -> Transformation v -> Render b v -> Render b v
  doRender :: b -> Options b v -> Render b v -> Result b v
  adjustDia ::
    Monoid m =>
    b -> Options b v -> AnnDiagram b v m -> AnnDiagram b v m
  renderDia ::
    (InnerSpace v, OrderedField (Scalar v), Monoid m) =>
    b -> Options b v -> AnnDiagram b v m -> Result b v
    -- Defined in Graphics.Rendering.Diagrams.Core
-}

module Diagrams.Backend.Image where

import Diagrams.Prelude
import qualified Diagrams.AST as AST

-- | Token for identifying this backend.
data ImageBackend = ImageBackend

instance HasLinearMap v => Backend ImageBackend v where
  data Render  ImageBackend v = SR AST.Image
  type Result  ImageBackend v = AST.Image
  data Options ImageBackend v = ImageOpts

  withStyle _ _ _      r = r -- XXX FIXME
  doRender  _ _ (SR r)   = r -- Ignore options for now

instance Monoid (Render ImageBackend v) where
  mempty  = undefined
  mappend = undefined
